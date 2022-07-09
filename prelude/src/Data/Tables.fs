namespace Prelude.Data.Tables

open System
open Percyqaz.Json
open Prelude.Data.Charts.Caching

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
type TableChange =
    | MoveChart of id: string * before_level: string * after_level: string
    | AddChart of id: string * level: string
    | RemoveChart of id: string * level: string

    | AddLevel of name: string
    | RemoveLevel of name: string
    | RenameLevel of oldname: string * newname: string

[<Json.AutoCodec>]
type Level =
    {
        /// Display name ingame
        Name: string
        Charts: ResizeArray<TableChart>
    }

[<Json.AutoCodec>]
type Table =
    {
        Name: string
        Levels: ResizeArray<Level>
        Changelog: ResizeArray<TableChange * DateTime>
    }
    member private this.Log(c: TableChange) =
        this.Changelog.Add (c, DateTime.UtcNow)

    // Levels

    member private this.Level(id: string) : Level = this.Levels.Find(fun l -> l.Name = id)

    member this.CreateLevel(id: string) =
        for l in this.Levels do
            if l.Name = id then failwith "A level with this id already exists"
        let l = { Name = id; Charts = ResizeArray() }
        this.Levels.Add l
        this.Log(AddLevel id)

    member this.RemoveLevel(id: string) =
        this.Levels.Remove(this.Level id) |> ignore
        this.Log(RemoveLevel id)

    member this.RenameLevel(id: string, new_id: string) =
        this.Log(RenameLevel (id, new_id))
        let l = this.Level id
        this.Levels.Remove l |> ignore
        this.Levels.Add { l with Name = new_id }

    // Charts

    member this.AddChart(level: string, cid: string, chart: CachedChart) =
        for l in this.Levels do
            for c in l.Charts do
                if c.Id = cid then failwith "A table entry with this id already exists"

        let l = this.Level level
        if l.Charts.TrueForAll (fun c -> c.Hash <> chart.Hash) then
            l.Charts.Add { Id = cid; Hash = chart.Hash }
            this.Log(AddChart (cid, l.Name))
        else failwith "Chart already added to this level"
        
    member this.RemoveChart(cid: string) =
        for l in this.Levels do
            match Seq.tryFind (fun c -> c.Id = cid) l.Charts with
            | Some c -> l.Charts.Remove c |> ignore; this.Log(RemoveChart (cid, l.Name))
            | None -> ()

    member this.MoveChart(cid: string, new_level: string) =
        for l in this.Levels do
            if l.Name <> new_level then
                match Seq.tryFind (fun c -> c.Id = cid) l.Charts with
                | Some c ->
                    l.Charts.Remove c |> ignore
                    (this.Level new_level).Charts.Add c
                    this.Log(MoveChart (cid, l.Name, new_level))
                | None -> ()

module Table =

    open Prelude.Common
    open System.IO
        
    let mutable current : Table option = None
    let mutable currentFile = ""

    let save() =
        match current with
        | Some t -> JSON.ToFile(Path.Combine(getDataPath "Data", "Tables", currentFile), true) t
        | None -> ()

    let load(fileid: string) =
        save()
        currentFile <- fileid + ".table"
        current <- Path.Combine(getDataPath "Data", "Tables", currentFile) |> JSON.FromFile |> function Ok t -> Some t | Result.Error e -> raise e

    let create(name: string, fileid: string) =
        save()
        if Path.Combine(getDataPath "Data", "Tables", currentFile) |> File.Exists then failwith "Table already exists"
        currentFile <- fileid + ".table"
        current <- Some { Name = name; Levels = ResizeArray(); Changelog = ResizeArray() }
        save()