namespace Backbeat.Features

open System.IO
open System.IO.Compression
open System.Diagnostics
open System.Linq
open Percyqaz.Shell
open Prelude.Common
open Prelude.Charts.Formats.Interlude
open Prelude.Data.Charts.Tables
open Prelude.Data.Charts.Library
open Prelude.Data.Charts.Collections
open Prelude.Data.Charts.Caching
open Backbeat.Utils

module Tables =

    // Editing/suggestion phase

    //let fetch_table(file: string) =
        
    //    let source = Path.Combine(INTERLUDE_TABLES_PATH, file + ".table")
    //    let target = Path.Combine(TABLES_PATH, file + ".table")

    //    if File.GetLastWriteTime target > File.GetLastWriteTime source then
    //        printfn "Source is older than target. Is this what you want?"
    //    else

    //    File.Delete target
    //    File.Copy (source, target)
    //    printfn "Copied from %s" source
    
    let put_table(file: string) =
        
        let target = Path.Combine(INTERLUDE_TABLES_PATH, file.Replace(".suggestions", "") + ".table")
        let source = Path.Combine(TABLES_PATH, file + ".table")

        printfn "Overwriting Interlude local table with %s ..." file

        File.Delete target
        File.Copy (source, target)
        printfn "Copied to %s" target

    let private flatten_table(file: string) =
        
        let table : Table = Path.Combine(TABLES_PATH, file + ".table") |> JSON.FromFile |> function Result.Ok t -> t | Error e -> raise e
        let flat = Path.Combine(TABLES_PATH, file + ".txt")

        let lines = ResizeArray<string>()

        for level in table.Levels do
            lines.Add(sprintf "# %s" level.Name)
            for chart in level.Charts do
                lines.Add chart.Id

        File.WriteAllLines(flat, lines)
        printfn "Flattened %s.table -> %s.txt" file file

    let private unflatten_table(file: string) =
        
        let table : Table = Path.Combine(TABLES_PATH, file + ".table") |> JSON.FromFile |> function Result.Ok t -> t | Error e -> raise e
        let flat = Path.Combine(TABLES_PATH, file + ".txt")
        
        let lines = File.ReadAllLines flat

        let newTable = { table with Levels = ResizeArray<Level>() }
        let mutable level = ""
        let mutable levelCharts = ResizeArray<TableChart>()

        for line in lines do
            if line.StartsWith "# " then
                level <- line.Substring 2
                levelCharts <- ResizeArray<TableChart>()
                newTable.Levels.Add { Charts = levelCharts; Name = level }
            else
                let chart : TableChart =
                    table.Levels
                        .Find(fun level -> level.Charts.Any(fun chart -> chart.Id = line))
                        .Charts
                        .Find(fun chart -> chart.Id = line)
                levelCharts.Add chart

        JSON.ToFile (Path.Combine(TABLES_PATH, file + ".table"), true) newTable
        printfn "Updated %s.table" file

        File.Delete flat

    let edit_table(table: string) =
        let suggestions = table + ".suggestions"
        flatten_table suggestions
        printfn "Waiting for text editor to exit.."
        Process.Start(ProcessStartInfo(Path.Combine(TABLES_PATH, suggestions + ".txt"), UseShellExecute = true)).WaitForExit()
        printfn "Press ENTER to finish editing!"
        System.Console.ReadLine() |> ignore
        printfn "Saving your changes :)"
        unflatten_table suggestions

    let add_suggestions (file: string) (folder: string) =

        let table : Table = Path.Combine(TABLES_PATH, file + ".suggestions.table") |> JSON.FromFile |> function Result.Ok t -> t | Error e -> raise e

        table.AddLevel("XXX") |> ignore

        let collections : Collections =
            match JSON.FromFile INTERLUDE_COLLECTIONS_FILE with
            | Result.Ok c -> c
            | Error e -> raise e

        match collections.GetFolder folder with
        | None -> 
            printfn "No such folder '%s' in your local Interlude" folder
            printfn "Local folders: %s" (String.concat ", " collections.Folders.Keys)
        | Some folder ->
            for entry in folder.Charts |> List.ofSeq do
                if table.Contains entry.Hash then printfn "Already in table: %s" entry.Path
                else
                    match Chart.fromFile entry.Path with
                    | None -> printfn "Failed loading %s" entry.Path
                    | Some chart -> 
                        let cc = cacheChart chart
                        let id = Table.generate_cid cc
                        if table.AddChart("XXX", id, cc) then 
                            printfn "+ %s" id
                            folder.Remove cc |> ignore
                        else printfn "Failed adding %s" id

        table |> JSON.ToFile(Path.Combine(TABLES_PATH, file + ".suggestions.table"), true)
        printfn "Saved table additions."
        collections |> JSON.ToFile(INTERLUDE_COLLECTIONS_FILE, true)
        printfn "Saved collections."

    // Commit phase

    type Action = 
        | Removed
        | Added
        | MovedHere of string
        | MovedAway of string

    let commit_table(file: string) =
        
        let new_table : Table = Path.Combine(TABLES_PATH, file + ".suggestions.table") |> JSON.FromFile |> function Result.Ok t -> t | Error e -> raise e
        let table : Table = Path.Combine(TABLES_PATH, file + ".table") |> JSON.FromFile |> function Result.Ok t -> t | Error e -> raise e

        new_table.RemoveLevel("XXX") |> ignore

        let diffs = ResizeArray<string * TableChart * Action>()
        for new_level in new_table.Levels do
            for chart in new_level.Charts do
                if table.Contains chart.Hash then
                    let level = table.LevelOf chart.Hash
                    if level.Name <> new_level.Name then
                        diffs.Add (level.Name, chart, MovedAway new_level.Name)
                        diffs.Add (new_level.Name, chart, MovedHere level.Name)
                else diffs.Add(new_level.Name, chart, Added)
        
        for level in table.Levels do
            for chart in level.Charts do
                if not (new_table.Contains chart.Hash) then
                    diffs.Add(level.Name, chart, Removed)

        if diffs.Count = 0 then 
            printfn "No changes to commit." 
            File.WriteAllText(Path.Combine(TABLES_PATH, file + ".diff"), "")
        else

        let diff_text = System.Text.StringBuilder()
        let write_diff (s: string) = diff_text.AppendLine(s) |> ignore

        sprintf "**Updates to %s table -- %s**" new_table.Name (System.DateTime.Now.ToString("dd/MM/yy", System.Globalization.CultureInfo.InvariantCulture)) |> write_diff

        for level, changes in Seq.groupBy(fun (l, _, _) -> l) diffs |> Seq.sortBy(fun (l, _) -> int <| l.Substring(2)) do
            sprintf "\n__%s__" level |> write_diff
            for (_, chart, action) in changes do
                match action with
                | Removed -> sprintf "- `%s`" chart.Id |> write_diff
                | Added -> sprintf "+ `%s`" chart.Id |> write_diff
                | MovedAway target -> sprintf "~ `%s` --> **%s**" chart.Id target |> write_diff
                | MovedHere from -> sprintf "~ `%s` <-- **%s**" chart.Id from |> write_diff

        let diff_text = diff_text.ToString()

        printfn "%s" diff_text

        printfn "Saving changelog."

        File.WriteAllText(Path.Combine(TABLES_PATH, file + ".diff"), diff_text)
        File.AppendAllText(Path.Combine(TABLES_PATH, file + ".changelog"), "\n" + diff_text)

        new_table |> JSON.ToFile (Path.Combine(TABLES_PATH, file + ".table"), true)

        printfn "Commited updates to table."

    let check_table(file: string) =
        let table : Table = Path.Combine(TABLES_PATH, file + ".table") |> JSON.FromFile |> function Result.Ok t -> t | Error e -> raise e

        for l in table.Levels do
            for c in l.Charts do
                if not (Archive.Storage.charts.ContainsKey c.Hash) then printfn "'%s' is not in the backbeat database" c.Id

    // Publishing the table

    let export_table_sources(file: string) =

        Directory.SetCurrentDirectory config.InterludePath
        
        let sources = ResizeArray<string>()
        let table : Table = Path.Combine(TABLES_PATH, file + ".table") |> JSON.FromFile |> function Result.Ok t -> t | Error e -> raise e
        for level in table.Levels do
            for chart in level.Charts do
                match lookupHash chart.Hash with
                | Some cc -> 
                    match load cc with
                    | Some c -> 
                        let source =
                            match c.Header.ChartSource with
                            | Unknown -> sprintf "Pack: %s" c.Header.SourcePack
                            | Osu (setid, mapid) -> sprintf "https://osu.ppy.sh/beatmapsets/%i#mania/%i" setid mapid
                            | Stepmania packid -> sprintf "https://etternaonline.com/pack/%i" packid
                        if sources.Contains source |> not then sources.Add source
                    | None -> printfn "Error loading chart: %s" chart.Id
                | None -> printfn "Chart missing from local library: %s" chart.Id

        File.WriteAllLines(Path.Combine(TABLES_PATH, file + ".sources.txt"), sources)
        printfn "Written to %s.sources.txt" file

    open SixLabors.ImageSharp

    let export_table_charts(file: string, skip: bool) =

        Directory.SetCurrentDirectory config.InterludePath
        
        let table : Table = Path.Combine(TABLES_PATH, file + ".table") |> JSON.FromFile |> function Result.Ok t -> t | Error e -> raise e
        for level in table.Levels do
            for chart in level.Charts do
                match lookupHash chart.Hash with
                | Some cc -> 
                    match load cc with
                    | Some c -> 
                        let path = Path.Combine(CHARTS_PATH, chart.Id.Replace("/", "_"))
                        if skip && Directory.Exists path then
                            printfn "Skipping %s" chart.Id
                        else
                        printfn "%s" chart.Id
                        if Directory.Exists path then Directory.Delete (path, true) |> ignore
                        Directory.CreateDirectory path |> ignore
                        let updated_chart = { c with Header = { c.Header with AudioFile = Relative "audio.mp3"; BackgroundFile = Relative "bg.png" }; LoadedFromPath = "" }
                        try
                            Chart.toFile updated_chart (Path.Combine(path, chart.Id.Split("/", 2).[1] + ".yav"))
                            File.Copy (c.AudioPath, Path.Combine(path, "audio.mp3"))
                            use bitmap = Bitmap.Load c.BackgroundPath
                            bitmap.SaveAsPng(Path.Combine(path, "bg.png"))
                        with err -> printfn "Error copying chart %s: %O" chart.Id err
                    | None -> printfn "Error loading chart: %s" chart.Id
                | None -> printfn "Chart missing from local library: %s" chart.Id
    
    let export_as_pack(file: string) =

        export_table_charts (file, true)

        printfn "\n GOT ALL THE CHARTS WE NEED \n"
            
        let table : Table = Path.Combine(TABLES_PATH, file + ".table") |> JSON.FromFile |> function Result.Ok t -> t | Error e -> raise e

        use fs = File.Open(Path.Combine(PACKS_PATH, file + ".zip"), FileMode.CreateNew)
        use zip = new ZipArchive(fs, ZipArchiveMode.Create)

        for level in table.Levels do
            for chart in level.Charts do
                let dirname = chart.Id.Replace("/", "_")
                printfn "%s" dirname
                let source_dir = Path.Combine(CHARTS_PATH, dirname)
                let filename = chart.Id.Split("/", 2).[1] + ".yav"
                zip.CreateEntryFromFile(Path.Combine(source_dir, filename), sprintf "%s/%s" dirname filename) |> ignore
                zip.CreateEntryFromFile(Path.Combine(source_dir, "audio.mp3"), sprintf "%s/audio.mp3" dirname) |> ignore
                zip.CreateEntryFromFile(Path.Combine(source_dir, "bg.png"), sprintf "%s/bg.png" dirname) |> ignore

    let register (ctx: Context) =
        ctx
            //.WithCommand("fetch", 
            //Command.create "Copies locally installed table to this repo" ["table"] <| Impl.Create(Types.str, fetch_table))
            .WithCommand("put", 
            Command.create "Copies repo table to your local interlude" ["table"] <| Impl.Create(Types.str, put_table))
            .WithCommand("add_batch", 
            Command.create "Add batch of table charts from a local collection" ["table"; "collection"] <| Impl.Create(Types.str, Types.str, add_suggestions))
            .WithCommand("edit", 
            Command.create "Make edits to existing charts in a table" ["table"] <| Impl.Create(Types.str, edit_table))
            .WithCommand("check_table", 
            Command.create "Verify all parts of the table are in backbeat" ["table"] <| Impl.Create(Types.str, check_table))
            .WithCommand("commit", 
            Command.create "Commit suggestions to table and generate a changelog" ["table"] <| Impl.Create(Types.str, commit_table))
            .WithCommand("sources", 
            Command.create "Export sources needed to have every chart for a table" ["table"] <| Impl.Create(Types.str, export_table_sources))
            .WithCommand("publish", 
            Command.create "Export Crescent as a .zip" ["table"] <| Impl.Create(Types.str, export_as_pack))
