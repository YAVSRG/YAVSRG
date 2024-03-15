namespace Backbeat.Features

open System.IO
open System.Diagnostics
open System.Linq
open Percyqaz.Common
open Percyqaz.Shell
open Prelude.Common
open Prelude.Backbeat
open Prelude.Data.Library.Collections
open Prelude.Data.Library.Caching
open Backbeat.Features.Archive
open Backbeat.Utils

// module Tables =

//     let interlude_cache = Cache.from_path INTERLUDE_SONGS_FOLDER

//     let put_table (file: string) =

//         let target =
//             Path.Combine(INTERLUDE_TABLES_PATH, file.Replace(".suggestions", "") + ".table")

//         let source = Path.Combine(TABLES_PATH, file + ".table")

//         printfn "Overwriting Interlude local table with %s ..." file

//         File.Delete target
//         File.Copy(source, target)
//         printfn "Copied to %s" target

//     let private flatten_table (file: string) =

//         let table: Table =
//             Path.Combine(TABLES_PATH, file + ".table")
//             |> JSON.FromFile
//             |> function
//                 | Result.Ok t -> t
//                 | Error e -> raise e

//         let flat = Path.Combine(TABLES_PATH, file + ".txt")

//         let lines = ResizeArray<string>()

//         for level in table.Levels do
//             lines.Add(sprintf "# %i\t%s" level.Rank level.Name)

//             for chart in level.Charts do
//                 lines.Add chart.Id

//         File.WriteAllLines(flat, lines)
//         printfn "Flattened %s.table -> %s.txt" file file

//     let private unflatten_table (file: string) =

//         let table: Table =
//             Path.Combine(TABLES_PATH, file + ".table")
//             |> JSON.FromFile
//             |> function
//                 | Result.Ok t -> t
//                 | Error e -> raise e

//         let flat = Path.Combine(TABLES_PATH, file + ".txt")

//         let lines = File.ReadAllLines flat

//         let new_table =
//             { table with
//                 Levels = ResizeArray<Level>()
//             }

//         let mutable levelCharts = ResizeArray<TableChart>()

//         for line in lines do
//             if line.StartsWith "# " then
//                 let split = line.Substring(2).Split([| '\t' |], 2)
//                 levelCharts <- ResizeArray<TableChart>()

//                 new_table.Levels.Add
//                     {
//                         Charts = levelCharts
//                         Name = split.[1]
//                         Rank = int split.[0]
//                     }
//             else
//                 let chart: TableChart =
//                     table.Levels
//                         .Find(fun level -> level.Charts.Any(fun chart -> chart.Id = line))
//                         .Charts.Find(fun chart -> chart.Id = line)

//                 levelCharts.Add chart

//         JSON.ToFile
//             (Path.Combine(TABLES_PATH, file + ".table"), true)
//             { new_table with
//                 Version = Table.generate_table_version ()
//             }

//         printfn "Updated %s.table" file

//         File.Delete flat

//     let edit_table (table: string) =
//         let suggestions = table + ".suggestions"
//         flatten_table suggestions
//         printfn "Waiting for text editor to exit.."

//         Process
//             .Start(ProcessStartInfo(Path.Combine(TABLES_PATH, suggestions + ".txt"), UseShellExecute = true))
//             .WaitForExit()

//         printfn "Press ENTER to finish editing!"
//         System.Console.ReadLine() |> ignore
//         printfn "Saving your changes :)"
//         unflatten_table suggestions

//     let add_suggestions (file: string) (folder: string) =

//         let table: Table =
//             Path.Combine(TABLES_PATH, file + ".suggestions.table")
//             |> JSON.FromFile
//             |> function
//                 | Ok t -> t
//                 | Error e -> raise e

//         table.AddLevel("XXX", -1) |> ignore

//         let collections: Collections =
//             match JSON.FromFile INTERLUDE_COLLECTIONS_FILE with
//             | Ok c -> c
//             | Error e -> raise e

//         match collections.GetFolder folder with
//         | None ->
//             printfn "No such folder '%s' in your local Interlude" folder
//             printfn "Local folders: %s" (String.concat ", " collections.Folders.Keys)
//         | Some folder ->
//             for entry in folder.Charts |> List.ofSeq do
//                 match Cache.by_key entry.Path interlude_cache with
//                 | Some cc ->
//                     let id = Table.generate_cid (cc.Creator, cc.Title)

//                     if table.AddChart(-1, id, cc.Keys, cc.Hash) then
//                         printfn "+ %s" id
//                         folder.Remove cc |> ignore
//                 | None -> printfn "Chart key '%s' from collection not in cache" entry.Path

//         { table with
//             Version = Table.generate_table_version ()
//         }
//         |> JSON.ToFile(Path.Combine(TABLES_PATH, file + ".suggestions.table"), true)

//         printfn "Saved table additions."
//         collections |> JSON.ToFile(INTERLUDE_COLLECTIONS_FILE, true)
//         printfn "Saved collections."

//     // Commit phase

//     type Action =
//         | Removed of string
//         | Added of string
//         | Moved of before: string * after: string

//     let commit_table (file: string) =

//         let new_table: Table =
//             Path.Combine(TABLES_PATH, file + ".suggestions.table")
//             |> JSON.FromFile
//             |> function
//                 | Result.Ok t -> t
//                 | Error e -> raise e

//         let table: Table =
//             Path.Combine(TABLES_PATH, file + ".table")
//             |> JSON.FromFile
//             |> function
//                 | Result.Ok t -> t
//                 | Error e -> raise e

//         new_table.RemoveLevel("XXX") |> ignore

//         let diffs = ResizeArray<TableChart * Action>()

//         for new_level in new_table.Levels do
//             for chart in new_level.Charts do
//                 if table.Contains chart.Hash then
//                     let level = (table.LevelOf chart.Hash).Value

//                     if level.Name <> new_level.Name then
//                         diffs.Add(chart, Moved(level.Name, new_level.Name))
//                 else
//                     diffs.Add(chart, Added new_level.Name)

//         for level in table.Levels do
//             for chart in level.Charts do
//                 if not (new_table.Contains chart.Hash) then
//                     diffs.Add(chart, Removed level.Name)

//         if diffs.Count = 0 then
//             printfn "No changes to commit."
//             File.WriteAllText(Path.Combine(TABLES_PATH, file + ".diff"), "")
//         else

//         let diff_text = System.Text.StringBuilder()
//         let write_diff (s: string) = diff_text.AppendLine(s) |> ignore

//         sprintf
//             "## Updates to %s table -- %s"
//             new_table.Name
//             (System.DateTime.UtcNow.ToString("dd/MM/yy", System.Globalization.CultureInfo.InvariantCulture))
//         |> write_diff

//         let mutable removals = []
//         let mutable additions = []
//         let mutable movements = []

//         for chart, action in diffs do
//             match action with
//             | Removed l -> removals <- sprintf "[%s] %s" l chart.Id :: removals
//             | Added l -> additions <- sprintf "[%s] %s" l chart.Id :: additions
//             | Moved(before, after) ->
//                 movements <- sprintf "[%s] %s -> [%s] %s" before chart.Id after chart.Id :: movements

//         if additions <> [] then
//             write_diff ""
//             write_diff "### New charts"
//             List.iter write_diff additions

//         if movements <> [] then
//             write_diff ""
//             write_diff "### Level changes"
//             List.iter write_diff movements

//         if removals <> [] then
//             write_diff ""
//             write_diff "### Removed charts"
//             List.iter write_diff removals

//         let diff_text = diff_text.ToString()

//         printfn "%s" diff_text

//         printfn "Saving changelog."

//         File.WriteAllText(Path.Combine(TABLES_PATH, file + ".diff"), diff_text)
//         File.AppendAllText(Path.Combine(TABLES_PATH, file + ".changelog"), "\n" + diff_text)

//         { new_table with
//             Version = Table.generate_table_version ()
//         }
//         |> JSON.ToFile(Path.Combine(TABLES_PATH, file + ".table"), true)

//         printfn "Commited updates to table."

//     let check_table (file: string) =
//         let table: Table =
//             Path.Combine(TABLES_PATH, file + ".table")
//             |> JSON.FromFile
//             |> function
//                 | Result.Ok t -> t
//                 | Error e -> raise e

//         for l in table.Levels do
//             for c in l.Charts do
//                 if not (charts.ContainsKey c.Hash) then
//                     printfn "'%s' is not in the backbeat database" c.Id

//     let upload_table_charts (file: string) =

//         let table: Table =
//             Path.Combine(TABLES_PATH, file + ".table")
//             |> JSON.FromFile
//             |> function
//                 | Result.Ok t -> t
//                 | Error e -> raise e

//         for level in table.Levels do
//             for chart in level.Charts do
//                 // if already in backbeat cache, skip
//                 match Cache.by_key (sprintf "%s/%s" table.Name chart.Hash) backbeat_cache with
//                 | Some _ -> ()
//                 | None ->

//                 // if in backbeat cache but not the right folder, make a copy
//                 match Cache.by_hash chart.Hash backbeat_cache with
//                 | Some cc ->
//                     Cache.copy table.Name cc backbeat_cache
//                     Logging.Info(sprintf "v^ %s" chart.Id)
//                 | None ->

//                 // otherwise look in Interlude client's cache for it
//                 match Cache.by_hash chart.Hash interlude_cache with
//                 | Some cc ->
//                     match Cache.load cc interlude_cache with
//                     | Some ch ->
//                         Logging.Info(sprintf "source for missing chart '%s': %A" chart.Id ch.Header.ChartSource)
//                     | None -> Logging.Error(sprintf "Error loading '%s'" chart.Id)
//                 | None -> Logging.Info(sprintf "Chart missing from both backbeat and your local client: %s" chart.Id)

//         Cache.save backbeat_cache
//         Collect.slurp_folder [] table.Name

//     let register (ctx: ShellContext) =
//         ctx
//             .WithCommand("put", "Copies repo table to your local interlude", "table", put_table)
//             .WithCommand(
//                 "add_batch",
//                 "Add batch of table charts from a local collection",
//                 "table",
//                 "collection",
//                 add_suggestions
//             )
//             .WithCommand("edit", "Make edits to existing charts in a table", "table", edit_table)
//             .WithCommand("check_table", "Verify all parts of the table are in backbeat", "table", check_table)
//             .WithCommand("upload_table", "Upload charts from table to backbeat storage", "table", upload_table_charts)
//             .WithCommand("commit", "Commit suggestions to table and generate a changelog", "table", commit_table)
