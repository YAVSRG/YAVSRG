namespace Prelude.Data.Charts.Tables

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

    member this.Rename(name: string) = this.Name <- name

[<Json.AutoCodec>]
type Table =
    {
        Name: string
        RulesetId: string
        Levels: ResizeArray<Level>
    }

    // Levels

    member private this.Level(id: string) : Level = this.Levels.Find(fun l -> l.Name = id)

    member this.AddLevel(id: string) =
        for l in this.Levels do
            if l.Name = id then failwith "A level with this id already exists"
        let l = { Name = id; Charts = ResizeArray() }
        this.Levels.Add l

    member this.RemoveLevel(id: string) =
        this.Levels.Remove(this.Level id) |> ignore

    // Charts

    member this.AddChart(level: string, cid: string, chart: CachedChart) =
        for l in this.Levels do
            for c in l.Charts do
                if c.Id = cid then failwith "A table entry with this id already exists"
                elif c.Hash = chart.Hash then failwith "A table entry with this hash already exists"

        let l = this.Level level
        if l.Charts.TrueForAll (fun c -> c.Hash <> chart.Hash) then
            l.Charts.Add { Id = cid; Hash = chart.Hash }
        else failwith "Chart already added to this level"
        
    member this.RemoveChart(cid: string) =
        for l in this.Levels do
            match Seq.tryFind (fun c -> c.Id = cid) l.Charts with
            | Some c -> l.Charts.Remove c |> ignore
            | None -> ()

    member this.MoveChart(cid: string, new_level: string) =
        for l in this.Levels do
            if l.Name <> new_level then
                match Seq.tryFind (fun c -> c.Id = cid) l.Charts with
                | Some c ->
                    l.Charts.Remove c |> ignore
                    (this.Level new_level).Charts.Add c
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
        current <- Path.Combine(getDataPath "Data", "Tables", currentFile) |> JSON.FromFile |> function Ok t -> Some t | Error e -> raise e

    let create(name: string, ruleset: Ruleset, fileid: string) =
        save()
        if Path.Combine(getDataPath "Data", "Tables", currentFile) |> File.Exists then failwith "Table already exists"
        currentFile <- fileid + ".table"
        current <- Some { Name = name; RulesetId = Ruleset.hash ruleset; Levels = ResizeArray() }
        save()