namespace Prelude.Data.Tables

open System
open Prelude.Data.Charts.Caching

type TableChart =
    {   
        /// (Human-friendly) unique identifier for this particular chart
        Id: string
        /// Reference by hash to a particular chart.
        /// In future, when you don't have the chart this hash will be used to look up song name and download information
        Hash: string
    }

type TableChange =
    | MoveChart of id: string * before_level: string * after_level: string
    | AddChart of id: string * level: string
    | RemoveChart of id: string * level: string

    | AddLevel of name: string
    | RemoveLevel of name: string
    | RenameLevel of oldname: string * newname: string

type Level =
    {
        /// Display name ingame
        Name: string
        Charts: TableChart list
    }

type Table =
    {
        Name: string
        Levels: Level list
        Changelog: (TableChange * DateTime) list
    }

module Table =
    
    let private log (ev: TableChange) (table: Table) =
        { table with
            Changelog = (ev, DateTime.UtcNow) :: table.Changelog
        }

    let pop_level (name: string) (table: Table) : Level * Level list =
        match table.Levels |> List.tryFind (fun level -> level.Name = name) with
        | Some level -> level, table.Levels |> List.except [level]
        | None -> failwithf "No level with name '%s'" name

    // Level editing

    let add_level (name: string) (table: Table) =
        match table.Levels |> List.tryFind (fun level -> level.Name = name) with
        | Some level -> failwithf "Level '%s' already exists" name
        | None ->

        { table with
            Levels =
                { Name = name; Charts = [] } :: table.Levels }
        |> log (AddLevel name)

    let remove_level (name: string) (table: Table) =
        let level, other_levels = pop_level name table
        { table with
            Levels = other_levels }
        |> log (RemoveLevel level.Name)

    let order_levels (names: string list) (table: Table) =
        let mutable table = table
        for name in List.rev names do
            let level, other_levels = pop_level name table
            table <- { table with Levels = level :: other_levels }
        table

    let rename_level (oldname: string) (newname: string) (table: Table) =
        let level, other_levels = pop_level oldname table
        { table with
            Levels = { level with Name = newname } :: other_levels }
        |> log (RenameLevel (level.Name, newname))

    // Chart editing

    let add_chart (chart: CachedChart) (id: string) (level_name: string) (table: Table) =
        let level, other_levels = pop_level level_name table
        
        match level.Charts |> List.tryFind (fun c -> c.Hash = chart.Hash) with
        | Some c -> failwithf "Chart '%s' already in this level" chart.Title
        | None ->

        { table with 
            Levels = { level with Charts = { Id = id; Hash = chart.Hash } :: level.Charts } :: other_levels }
        |> log (AddChart (id, level.Name))

    let remove_chart (chart: CachedChart) (table: Table) =
        
        let mutable found_level = None
        let mutable found_chart = None
        for level in table.Levels do
            for ch in level.Charts do
                if ch.Hash = chart.Hash then
                    found_level <- Some level
                    found_chart <- Some ch
        
        if found_level.IsNone then failwith "Chart doesn't appear in the table"

        let level, other_levels = pop_level found_level.Value.Name table

        { table with 
            Levels = { level with Charts = List.except [found_chart.Value] level.Charts } :: other_levels }
        |> log (RemoveChart (found_chart.Value.Id, level.Name))
