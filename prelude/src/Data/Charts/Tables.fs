namespace Prelude.Data.Charts.Tables

open System.Linq
open Percyqaz.Common
open Percyqaz.Json
open Prelude.Data.Charts.Caching
open Prelude.Gameplay

[<Json.AutoCodec>]
type TableChart =
    {   
        /// (Human-friendly) unique identifier for this particular chart
        Id: string
        /// Reference by hash to a particular chart.
        /// When you don't have the chart this hash will be used to look up song name and download information
        Hash: string
    }

[<Json.AutoCodec>]
type Level =
    {
        /// Display name ingame
        mutable Name: string
        Rank: int
        Charts: ResizeArray<TableChart>
    }

    member this.Contains(chart: CachedChart) =
        this.Charts |> Seq.forall (fun c -> c.Hash <> chart.Hash) |> not

[<Json.AutoCodec>]
type Table =
    {
        Name: string
        Keymode: int
        RulesetId: string
        Levels: ResizeArray<Level>
        Version: int
    }

    // Levels

    member private this.Level(id: string) : Level = this.Levels.Find(fun l -> l.Name = id)
    member this.TryLevel(id: string) : Level option = Seq.tryFind (fun (l: Level) -> l.Name = id) this.Levels

    member this.AddLevel(id: string, rank: int) : bool =
        if id = "" then false else

        if Seq.forall (fun (l: Level) -> l.Name <> id && l.Rank <> rank) this.Levels |> not then false else

        let l = { Name = id; Charts = ResizeArray(); Rank = rank }
        this.Levels.Add l
        true

    member this.RemoveLevel(id: string) =
        this.Levels.Remove(this.Level id)

    member this.RenameLevel(old_name: string, new_name: string) =
        if new_name = "" then false else

        if this.TryLevel(new_name).IsSome then false
        else this.Level(old_name).Name <- new_name; true

    // Charts

    member this.LevelOf(hash: string) : Level =
        this.Levels.Find(fun l -> l.Charts.Any(fun c -> c.Hash = hash))

    member this.LevelOf(cc: CachedChart) : Level = this.LevelOf cc.Hash

    member this.Contains(hash: string) : bool =
        this.Levels.Any(fun l -> l.Charts.Any(fun c -> c.Hash = hash))

    member this.Contains(cc: CachedChart) : bool = this.Contains cc.Hash

    member this.AddChart(level: string, cid: string, chart: CachedChart) : bool =
        if
            this.Levels
            |> Seq.forall (fun l -> l.Charts |> Seq.forall (fun c -> c.Id <> cid && c.Hash <> chart.Hash))
            |> not
        then false else

        if chart.Keys <> this.Keymode then false else

        let l = this.Level level
        if l.Charts.TrueForAll (fun c -> c.Hash <> chart.Hash) then
            l.Charts.Add { Id = cid; Hash = chart.Hash }
            true
        else false
        
    member this.RemoveChart(chart: CachedChart) : bool =
        let mutable removed = false
        for l in this.Levels do
            match Seq.tryFind (fun c -> c.Hash = chart.Hash) l.Charts with
            | Some c -> removed <- l.Charts.Remove c
            | None -> ()
        removed

module Table =

    type Loaded = { File: string; Table: Table }

    open Prelude.Common
    open System.IO
        
    let mutable private _current : Loaded option = None

    let current() = 
        match _current with Some l -> Some l.Table | _ -> None

    let private loaded  = ResizeArray<Loaded>()
    
    let generate_cid(chart: CachedChart) =
        let strip =
            let regex = System.Text.RegularExpressions.Regex("[^\sa-zA-Z0-9_-]")
            fun (s: string) -> regex.Replace(s.ToLowerInvariant(), "").Trim().Replace(" ", "-")
        sprintf "%s/%s" (strip chart.Creator) (strip chart.Title)
    
    let generate_table_name(name: string) =
        let strip =
            let regex = System.Text.RegularExpressions.Regex("[^\sa-zA-Z0-9_-]")
            fun (s: string) -> regex.Replace(s.ToLowerInvariant(), "").Trim().Replace(" ", "-")
        strip name

    let generate_table_version() = System.DateTime.UtcNow.ToString("yyyyMMdd") |> int

    let table_path () = Path.Combine(getDataPath "Data", "Tables")
    let table_pathf f = Path.Combine(getDataPath "Data", "Tables", f)

    let load(name: string) =
        match loaded |> Seq.tryFind (fun l -> l.Table.Name = name) with
        | Some l -> _current <- Some l
        | None -> Logging.Warn(sprintf "Table not found: '%s'" name)

    let init(selected: string option) =
        loaded.Clear()
        let table_path = Path.Combine(getDataPath "Data", "Tables")
        Directory.CreateDirectory table_path |> ignore
        for file in Directory.GetFiles table_path do
            if Path.GetExtension(file).ToLower() = ".table" then
                match JSON.FromFile file with
                | Ok t -> loaded.Add({ File = Path.GetFileName file; Table = t})
                | Error e -> Logging.Error(sprintf "Error loading table: %s" (Path.GetFileName file), e)
        Logging.Info(sprintf "Loaded %i tables." loaded.Count)
        match selected with
        | Some selected -> load selected
        | None -> ()

    let install(id: string, table: Table) =
        JSON.ToFile(table_pathf id, true) table
        init(Some table.Name)

    let list() = loaded :> Loaded seq

    let save() =
        match _current with
        | Some t -> JSON.ToFile(table_pathf t.File, true) t.Table
        | None -> ()

    let create(name: string, keymode: int, ruleset: Ruleset) : bool =
        if name = "" then false else

        save()
        let fileid = generate_table_name name + ".table"
        if table_pathf fileid |> File.Exists || loaded.Any(fun l -> l.Table.Name = name) then false else
        _current <- 
            Some { 
                File = fileid
                Table = 
                    { 
                        Name = name
                        Keymode = keymode
                        RulesetId = Ruleset.hash ruleset
                        Levels = ResizeArray()
                        Version = generate_table_version()
                    }
            }
        loaded.Add _current.Value
        save()
        true

    open Prelude.Data.Scores

    let ratings() =
        if _current.IsNone then Seq.empty else
        seq {
            for level in _current.Value.Table.Levels do
                for chart in level.Charts do
                    let grade_achieved = 
                        match Scores.getData chart.Hash with
                        | Some d -> 
                            if d.Bests.ContainsKey(_current.Value.Table.RulesetId) then
                                let bests = d.Bests.[_current.Value.Table.RulesetId]
                                let grade, rate = bests.Grade.Best
                                if rate >= 1.0f then Some grade
                                else 
                                    let grade2, rate2 = bests.Grade.Fastest
                                    if rate2 >= 1.0f then Some grade2
                                    else None
                            else None
                        | None -> None
                    match grade_achieved with
                    | None -> ()
                    | Some -1 | Some 0 -> yield chart, 0.0
                    | Some 1 -> yield chart, max 0.0 (float level.Rank - 5.0) // C-
                    | Some 2 -> yield chart, max 0.0 (float level.Rank - 4.0) // C
                    | Some 3 -> yield chart, max 0.0 (float level.Rank - 3.0) // B-
                    | Some 4 -> yield chart, max 0.0 (float level.Rank - 2.0) // B
                    | Some 5 -> yield chart, max 0.0 (float level.Rank - 1.0) // A-
                    | Some 6 -> yield chart, float level.Rank // A
                    | Some 7 -> yield chart, float level.Rank + 0.5 // A+
                    | Some 8 -> yield chart, float level.Rank + 1.0 // S-
                    | Some 9 -> yield chart, float level.Rank + 1.5 // S
                    | Some 10 -> yield chart, float level.Rank + 2.0 // S+
                    | _ -> yield chart, 0.0
        } |> Seq.sortByDescending snd

[<Json.AutoCodec>]
type TableIndexEntry = { Name: string; Description: string; File: string; Version: int }
[<Json.AutoCodec>]
type TableIndex = { Tables: ResizeArray<TableIndexEntry> }