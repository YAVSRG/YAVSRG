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

    member this.Contains(chart: CachedChart) =
        this.Charts |> Seq.forall (fun c -> c.Hash <> chart.Hash) |> not

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

    member this.AddChart(level: string, cid: string, chart: CachedChart) : bool =
        if
            this.Levels
            |> Seq.forall (fun l -> l.Charts |> Seq.forall (fun c -> c.Id <> cid && c.Hash <> chart.Hash))
            |> not
        then false else

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

    let generate_cid(chart: CachedChart) =
        let strip =
            let regex = System.Text.RegularExpressions.Regex("[^\sa-zA-Z0-9_-]")
            fun (s: string) -> regex.Replace(s.ToLowerInvariant(), "").Trim().Replace(" ", "-")
        sprintf "%s/%s" (strip chart.Creator) (strip chart.Title)