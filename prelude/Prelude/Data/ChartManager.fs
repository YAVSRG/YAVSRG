module Prelude.Data.ChartManager

open System
open System.IO
open System.Collections.Generic
open Prelude.Json
open Prelude.Common
open Prelude.Charts.Interlude
open Prelude.Charts.ChartConversions
open Prelude.Gameplay.Mods
open Prelude.Gameplay.Layout
open Prelude.Gameplay.Difficulty

(*
    Caching of charts
*)

type CachedChart = {
    FilePath: string
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

    Collection: string
    CollectionIndex: int }

let cacheChart (chart : Chart) : CachedChart =
    let endTime =
        if chart.Notes.Count = 0 then 0.0 else
            chart.Notes.GetPointAt infinity |> offsetOf
    let rating = RatingReport(chart.Notes, 1.0, Layout.Spread, chart.Keys)
    {
    FilePath = chart.FileIdentifier
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

type PlaylistData = string * ModState * float
type Collection =
    | Collection of List<string>
    | Playlist of List<PlaylistData>
    | Goals of List<PlaylistData * Goal>
    (*static member ToJson (this: Collection) = json {
        match this with
        | Collection l -> do! JsonHelper.write "Collection" l
        | Playlist l -> do! Json.write "Playlist" l
        | Goals l -> do! Json.write "Goals" l
    }
    static member FromJson(_) =
        function
        | Property "Collection" l as json -> Json.init (Collection l) json
        | Property "Playlist" l as json -> Json.init (Playlist l) json
        | Property "Goals" l as json -> Json.init (Goals l) json
        | json -> Json.error "couldn't deserialize collection" json*)

type Cache() =
    let charts, collections = Cache.Load

    member this.Save = JsonHelper.saveFile (charts, collections) (Path.Combine(getDataPath("Data"), "Cache.json"))

    static member Load =
        try
            JsonHelper.loadFile(Path.Combine(getDataPath("Data"), "Cache.json"))
        with
        | :? FileNotFoundException -> (new Dictionary<string, CachedChart>(), new Dictionary<string, Collection>())
        | err -> Logging.Critical("Could not load cache file! Creating from scratch") (err.ToString()); (new Dictionary<string, CachedChart>(), new Dictionary<string, Collection>())
    
    member this.CacheChart (c: Chart) = lock(this) (fun () -> charts.[c.FileIdentifier] <- cacheChart c)

    member this.Count = charts.Count

    member this.LookupChart (id : string) : CachedChart option = if charts.ContainsKey(id) then Some charts.[id] else None

    member this.LoadChart (cc : CachedChart) : Chart option = 
        match cc.FilePath |> loadChartFile with
        | Some c ->
            this.CacheChart c
            Some c
        | None -> None

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

    member this.RebuildCache : LoggableTask =
        fun output ->
        (
            lock this (fun _ -> ( charts.Clear(); ))
            for pack in Directory.EnumerateDirectories(getDataPath "Songs") do
                for song in Directory.EnumerateDirectories(pack) do
                    for file in Directory.EnumerateFiles(song) do
                        match Path.GetExtension(file).ToLower() with
                        | ".yav" ->
                            lock this (fun _ ->
                            (
                                match loadChartFile(file) with
                                | Some c ->
                                    output("Caching " + file)
                                    this.CacheChart(c)
                                | None -> ()
                            ))
                        | _ -> ()
            this.Save
            output("Saved cache.")
            true
        )

    member this.DeleteChart (c : CachedChart) = failwith "nyi"

    member this.DeleteCharts (cs : List<CachedChart>) = Seq.iter (this.DeleteChart) cs

    member this.ConvertSongFolder path packname : LoggableTask =
        fun output ->
        (
            for file in Directory.EnumerateFiles(path) do
                match file with
                | ChartFile ->
                    output("Converting " + file)
                    loadAndConvertFile file
                    |> List.map (fun c -> relocateChart c path (Path.Combine(getDataPath "Songs", packname, Path.GetFileName(path))))
                    |> fun charts ->
                        lock this (fun _ ->
                        (
                            List.iter this.CacheChart charts
                        ))
                | _ -> ()
            true
        )

    member this.ConvertPackFolder path packname =
        fun output ->
        (
            Directory.EnumerateDirectories(path)
            //conversion of song folders one by one.
            //todo: test performance of converting in parallel (would create hundreds of tasks)
            |> Seq.iter (fun song -> (this.ConvertSongFolder song packname output |> ignore))
            true
        )

    member this.AutoConvert path : LoggableTask =
        fun output ->
        (
            match File.GetAttributes(path) &&& FileAttributes.Directory |> int with
            | 0 ->
                match path with
                | ChartFile -> failwith "single chart file not yet supported"
                | ChartArchive -> failwith "chart archives not yet supported"
                | _ -> failwith "unrecognised file"
            | _ ->
                match path with
                | SongFolder ->
                    //todo: replace ignore callback with action to refresh level select with new charts
                    //todo: maybe make "Singles" not a hardcoded string
                    TaskManager.AddTask ("Import " + Path.GetFileName(path), this.ConvertSongFolder path "Singles", ignore, true)
                | PackFolder ->
                    let packname =
                        match Path.GetFileName(path) with
                        | "Songs" -> if path |> Path.GetDirectoryName |> Path.GetFileName = "osu!" then "osu!" else "Songs"
                        | s -> s
                    //todo: replace ignore callback with action to refresh level select with new charts
                    TaskManager.AddTask ("Import pack " + Path.GetFileName(path), this.ConvertPackFolder path packname, ignore, true)
                | FolderOfPacks -> ()
                | _ -> failwith "no importable folder structure detected"
            true
        )