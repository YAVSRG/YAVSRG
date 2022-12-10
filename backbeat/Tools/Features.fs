namespace Interlude.Charts

open System.IO
open System.IO.Compression
open System.Linq
open Percyqaz.Shell
open Prelude.Common
open Prelude.Charts.Formats.Interlude
open Prelude.Data.Charts.Tables
open Prelude.Data.Charts.Library
open Utils

module Features =

    let fetch_table(file: string) =
        
        let source = Path.Combine(INTERLUDE_TABLES_PATH, file + ".table")
        let target = Path.Combine(TABLES_PATH, file + ".table")

        if File.GetLastWriteTime target > File.GetLastWriteTime source then
            printfn "Source is older than target. Is this what you want?"
        else

        File.Delete target
        File.Copy (source, target)
        printfn "Copied from %s" source
    
    let put_table(file: string) =
        
        let target = Path.Combine(INTERLUDE_TABLES_PATH, file + ".table")
        let source = Path.Combine(TABLES_PATH, file + ".table")

        if File.GetLastWriteTime target > File.GetLastWriteTime source then
            printfn "Source is older than target. Is this what you want?"
        else

        File.Delete target
        File.Copy (source, target)
        printfn "Copied to %s" target

    let flatten_table(file: string) =
        
        let table : Table = Path.Combine(TABLES_PATH, file + ".table") |> JSON.FromFile |> function Result.Ok t -> t | Error e -> raise e
        let flat = Path.Combine(TABLES_PATH, Path.ChangeExtension(file, ".txt"))

        let lines = ResizeArray<string>()

        for level in table.Levels do
            lines.Add(sprintf "# %s" level.Name)
            for chart in level.Charts do
                lines.Add chart.Id

        File.WriteAllLines(flat, lines)
        printfn "Flattened %s.table -> %s.txt" file file

    let unflatten_table(file: string) =
        
        let table : Table = Path.Combine(TABLES_PATH, file + ".table") |> JSON.FromFile |> function Result.Ok t -> t | Error e -> raise e
        let flat = Path.Combine(TABLES_PATH, Path.ChangeExtension(file, ".txt"))
        
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

        File.WriteAllLines(Path.Combine(TABLES_PATH, file + "-sources.txt"), sources)
        printfn "Written to %s-sources.txt" file

    open SixLabors.ImageSharp

    let export_table_charts(file: string) =

        Directory.SetCurrentDirectory config.InterludePath
        
        let table : Table = Path.Combine(TABLES_PATH, file + ".table") |> JSON.FromFile |> function Result.Ok t -> t | Error e -> raise e
        for level in table.Levels do
            for chart in level.Charts do
                match lookupHash chart.Hash with
                | Some cc -> 
                    match load cc with
                    | Some c -> 
                        printfn "%s" chart.Id
                        let path = Path.Combine(CHARTS_PATH, chart.Id.Replace("/", "_"))
                        Directory.Delete (path, true) |> ignore
                        Directory.CreateDirectory path |> ignore
                        let updated_chart = Chart(c.Keys, { c.Header with AudioFile = Relative "audio.mp3"; BackgroundFile = Relative "bg.png" }, c.Notes, c.BPM, c.SV, "")
                        try
                            Chart.toFile updated_chart (Path.Combine(path, chart.Id.Split("/", 2).[1] + ".yav"))
                            File.Copy (c.AudioPath, Path.Combine(path, "audio.mp3"))
                            use bitmap = Bitmap.Load c.BackgroundPath
                            bitmap.SaveAsPng(Path.Combine(path, "bg.png"))
                        with err -> printfn "Error copying chart %s: %O" chart.Id err
                    | None -> printfn "Error loading chart: %s" chart.Id
                | None -> printfn "Chart missing from local library: %s" chart.Id
    
    let export_crescent_packs() =
    
        Directory.SetCurrentDirectory config.InterludePath
            
        let crescent : Table = Path.Combine(TABLES_PATH, "crescent.table") |> JSON.FromFile |> function Result.Ok t -> t | Error e -> raise e

        use fs = File.Open(Path.Combine(PACKS_PATH, "crescent-draft001.zip"), FileMode.CreateNew)
        use zip = new ZipArchive(fs, ZipArchiveMode.Create)

        for level in crescent.Levels do
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
            .WithCommand("fetch_table", 
            Command.create "Copies local table to this repo" ["table"] <| Impl.Create(Types.str, fetch_table))
            .WithCommand("put_table", 
            Command.create "Copies repo table to your local interlude" ["table"] <| Impl.Create(Types.str, put_table))
            .WithCommand("flatten_table", 
            Command.create "Flattens a table to a human-readable/editable file" ["table"] <| Impl.Create(Types.str, flatten_table))
            .WithCommand("unflatten_table", 
            Command.create "Unflattens a human-readable table to the Interlude format" ["table"] <| Impl.Create(Types.str, unflatten_table))
            .WithCommand("export_table_sources", 
            Command.create "Export sources needed to have every chart for a table" ["table"] <| Impl.Create(Types.str, export_table_sources))
            .WithCommand("export_table_charts", 
            Command.create "Export charts for a table" ["table"] <| Impl.Create(Types.str, export_table_charts))
            .WithCommand("export_crescent_packs", 
            Command.create "Export Crescent as a .zip" [] <| Impl.Create(export_crescent_packs))
