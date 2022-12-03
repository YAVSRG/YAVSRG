namespace Prelude.Data.Charts.Tables

open System.Linq
open Percyqaz.Common
open Percyqaz.Json
open Prelude.Data.Charts.Caching
open Prelude.Scoring

[<Json.AutoCodec>]
type TableChart =
    {   
        /// (Human-friendly) unique identifier for this particular chart
        Id: string
        /// Reference by hash to a particular chart.
        /// In future, when you don't have the chart this hash will be used to look up song name and download information
        Hash: string
    }

[<Json.AutoCodec>]
type Level =
    {
        /// Display name ingame
        mutable Name: string
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
    }

    // Levels

    member private this.Level(id: string) : Level = this.Levels.Find(fun l -> l.Name = id)
    member private this.TryLevel(id: string) : Level option = Seq.tryFind (fun (l: Level) -> l.Name = id) this.Levels

    member this.AddLevel(id: string) : bool =
        if Seq.forall (fun (l: Level) -> l.Name <> id) this.Levels |> not then false else

        let l = { Name = id; Charts = ResizeArray() }
        this.Levels.Add l
        true

    member this.RemoveLevel(id: string) =
        this.Levels.Remove(this.Level id)

    member this.RenameLevel(old_name: string, new_name: string) =
        if this.TryLevel(new_name).IsSome then false
        else this.Level(old_name).Name <- new_name; true

    // Charts

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

    type private Loaded = { File: string; Table: Table }

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

    let table_path () = Path.Combine(getDataPath "Data", "Tables")
    let table_pathf f = Path.Combine(getDataPath "Data", "Tables", f)

    let load(name: string) =
        match loaded |> Seq.tryFind (fun l -> l.Table.Name = name) with
        | Some l -> _current <- Some l
        | None -> Logging.Warn(sprintf "Table not found: '%s'" name)

    let init(selected: string option) =
        for file in Directory.GetFiles(Path.Combine(getDataPath "Data", "Tables")) do
            if Path.GetExtension(file).ToLower() = ".table" then
                match JSON.FromFile file with
                | Ok t -> loaded.Add({ File = Path.GetFileName file; Table = t})
                | Error e -> Logging.Error(sprintf "Error loading table: %s" (Path.GetFileName file), e)
        Logging.Info(sprintf "Loaded %i tables." loaded.Count)
        match selected with
        | Some selected -> load selected
        | None -> ()

    let list() = loaded |> Seq.map(fun l -> l.Table.Name)

    let save() =
        match _current with
        | Some t -> JSON.ToFile(table_pathf t.File, true) t.Table
        | None -> ()

    let create(name: string, keymode: int, ruleset: Ruleset) : bool =
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
                    }
            }
        loaded.Add _current.Value
        save()
        true