namespace Interlude.Charts

open System.IO
open System.Linq
open Percyqaz.Shell
open Prelude.Common
open Prelude.Data.Charts.Tables
open Utils

module Features =

    let fetch_table(file: string) =
        
        let source = Path.Combine(INTERLUDE_TABLES_PATH, file)
        let target = Path.Combine(TABLES_PATH, file)

        if File.GetLastWriteTime target > File.GetLastWriteTime source then
            printfn "Source is older than target. Is this what you want?"
        else

        File.Delete target
        File.Copy (source, target)
    
    let put_table(file: string) =
        
        let target = Path.Combine(INTERLUDE_TABLES_PATH, file)
        let source = Path.Combine(TABLES_PATH, file)

        if File.GetLastWriteTime target > File.GetLastWriteTime source then
            printfn "Source is older than target. Is this what you want?"
        else

        File.Delete target
        File.Copy (source, target)

    let flatten_table(file: string) =
        
        let table : Table = Path.Combine(TABLES_PATH, file) |> JSON.FromFile |> function Result.Ok t -> t | Error e -> raise e
        let flat = Path.Combine(TABLES_PATH, Path.ChangeExtension(file, ".txt"))

        let lines = ResizeArray<string>()

        for level in table.Levels do
            lines.Add(sprintf "# %s" level.Name)
            for chart in level.Charts do
                lines.Add chart.Id

        File.WriteAllLines(flat, lines)

    let unflatten_table(file: string) =
        
        let table : Table = Path.Combine(TABLES_PATH, file) |> JSON.FromFile |> function Result.Ok t -> t | Error e -> raise e
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

        printfn "overwriting table"
        JSON.ToFile (Path.Combine(TABLES_PATH, file), true) newTable

    let register (ctx: Context) =
        ctx
            .WithCommand("fetch_table", 
            Command.create "Copies local table to this repo" ["file"] <| Impl.Create(Types.str, fetch_table))
            .WithCommand("put_table", 
            Command.create "Copies repo table to your local interlude" ["file"] <| Impl.Create(Types.str, put_table))
            .WithCommand("flatten_table", 
            Command.create "Flattens a table to a human-readable/editable file" ["file"] <| Impl.Create(Types.str, flatten_table))
            .WithCommand("unflatten_table", 
            Command.create "Unflattens a human-readable table to the Interlude format" ["file"] <| Impl.Create(Types.str, unflatten_table))
