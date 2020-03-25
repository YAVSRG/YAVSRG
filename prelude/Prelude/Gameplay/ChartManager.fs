module Prelude.Charts.ChartManager

open System
open System.IO
open System.Collections.Generic
open Newtonsoft.Json
open Prelude.Common
open Prelude.Charts.Interlude
open Prelude.Charts.ChartConversions
open Prelude.Gameplay.Score
open Prelude.Gameplay.Difficulty

(*
    Caching of charts
*)

type CachedChart = {
    File: string
    SourcePath: string
    Title: string
    Artist: string
    Creator: string
    Pack: string
    Hash: string
    Keys: int
    Length: float
    BPM: (float * float)
    DiffName: string
    Physical: float
    Technical: float

    [<JsonIgnore>]
    Collection: string
    [<JsonIgnore>]
    CollectionIndex: int }

let cacheChart (chart : Chart) : CachedChart =
    let endTime =
        if chart.Notes.Count = 0 then 0.0 else
            chart.Notes.GetPointAt infinity |> offsetOf
    let rating = RatingReport(chart.Notes, 1.0, Layout.Spread, chart.Keys)
    {
    File = chart.Header.File
    SourcePath = chart.Header.SourcePath
    Title = chart.Header.Title
    Artist = chart.Header.Artist
    Creator = chart.Header.Creator
    Pack = chart.Header.SourcePack
    Hash = calculateHash chart
    Keys = chart.Keys
    Length = if endTime = 0.0 then 0.0 else endTime - (offsetOf chart.Notes.First)
    BPM = minMaxBPM (chart.BPM.Enumerate |> List.ofSeq) endTime
    DiffName = chart.Header.DiffName
    Physical = rating.Physical
    Technical = rating.Technical
    Collection = ""
    CollectionIndex = 0 }

let osuSongFolder = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData), "osu!", "Songs")
let smPackFolder = Path.Combine(Path.GetPathRoot(Environment.CurrentDirectory), "Games", "Stepmania 5", "Songs")
let etternaPackFolder = Path.Combine(Path.GetPathRoot(Environment.CurrentDirectory), "Games", "Etterna", "Songs")

type ChartGroup = (string * CachedChart list)

type Goal =
    | NoGoal
    | Clear of unit
    | Grade of unit * int
    | Accuracy of unit * float

    //todo: replace unit type with mod list type
type PlaylistData = string * float * unit
type Collection =
    | Collection of List<string>
    | Playlist of List<PlaylistData>
    | Goals of List<PlaylistData * Goal>

type Cache() =
    let charts = new Dictionary<string, CachedChart>()
    let collections = new Dictionary<string, Collection>()
    
    member this.CacheChart (c: Chart) = lock(this) (fun () -> charts.[c.FileIdentifier] <- cacheChart c)

    member this.Count = charts.Count

    member this.LookupChart (id : string) : CachedChart option = if charts.ContainsKey(id) then Some charts.[id] else None

    member this.LoadChart (cc : CachedChart) : Chart option =  
        let id = Path.Combine(cc.SourcePath, cc.File)
        try
            let c = id |> loadChartFile
            this.CacheChart c
            Some c
        with
        | err -> Logging.Error ("Could not load chart from " + id) (err.ToString()); None

    member this.GetGroups grouping (sorting : Comparison<CachedChart>) =
        let groups = new Dictionary<string, List<CachedChart>>()
        for c in charts.Values do
            let s = grouping(c)
            if (groups.ContainsKey(s) |> not) then groups.Add(s, new List<CachedChart>())
            groups.[s].Add(c)
        for g in groups.Values do
            g.Sort(sorting)
        groups

    member this.GetCollections (sorting : Comparison<CachedChart>) = 
        let groups = new Dictionary<string, List<CachedChart>>()
        for name in collections.Keys do
            let c = collections.[name]
            match c with
            | Collection ids ->
                Seq.choose this.LookupChart ids
            | Playlist ps ->
                Seq.choose (fun (i, _, _) -> this.LookupChart i) ps
            | Goals gs ->
                Seq.choose (fun ((i, _, _), _) -> this.LookupChart i) gs
            |> ResizeArray<CachedChart>
            |> fun x -> groups.Add(name, x)
        for g in groups.Values do
            g.Sort(sorting)
        groups

    member this.RebuildCache = failwith "nyi"

    member this.DeleteChart (c : CachedChart) = failwith "nyi"

    member this.DeleteCharts (cs : List<CachedChart>) = failwith "nyi"

    member this.ConvertSongFolder path target = failwith "nyi"

    member this.ConvertPackFolder path target = failwith "nyi"

    member this.ConvertFolder path target = 
        //auto detect:
            // osz
            // zip containing sm, yav, ssc, osu
            // song folder
            // pack of song folders
            // folder of packs
        //accordingly extract if needed
        //accordingly convert
        failwith "nyi"

