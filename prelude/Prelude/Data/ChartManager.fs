namespace Prelude.Data

open System
open System.IO
open System.IO.Compression
open System.Collections.Generic
open Percyqaz.Json
open Prelude.Common
open Prelude.Charts.Interlude
open Prelude.Charts.ChartConversions
open Prelude.Gameplay.Mods
open Prelude.Gameplay.Layout
open Prelude.Gameplay.Difficulty

module ChartManager =

    (*
        Caching of charts
    *)
    [<Json.AllRequired>]
    type CachedChart = {
        FilePath: string
        Title: string
        Artist: string
        Creator: string
        Pack: string
        Hash: string
        Keys: int
        Length: Time
        BPM: (float32<ms/beat> * float32<ms/beat>)
        DiffName: string
        Physical: float
        Technical: float
    }
    with static member Default = { FilePath = ""; Title = ""; Artist = ""; Creator = ""; Pack = ""; Hash = ""; Keys = 4; Length = 0.0f<ms>; BPM = (0.0f<ms/beat>, 0.0f<ms/beat>); DiffName = ""; Physical = 0.0; Technical = 0.0 }

    let cacheChart (chart : Chart) : CachedChart =
        let endTime =
            if chart.Notes.Count = 0 then 0.0f<ms> else
                chart.Notes.GetPointAt (infinityf * 1.0f<ms>) |> offsetOf
        let rating = RatingReport(chart.Notes, 1.0f, Layout.Spread, chart.Keys)
        {
        FilePath = chart.FileIdentifier
        Title = chart.Header.Title
        Artist = chart.Header.Artist
        Creator = chart.Header.Creator
        Pack = chart.Header.SourcePack
        Hash = calculateHash chart
        Keys = chart.Keys
        Length = if endTime = 0.0f<ms> then 0.0f<ms> else endTime - (offsetOf <| chart.Notes.First())
        BPM = minMaxBPM (chart.BPM.Data |> List.ofSeq) endTime
        DiffName = chart.Header.DiffName
        Physical = rating.Physical
        Technical = rating.Technical }

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

    //todo: add reverse lookup from hash -> id and remove id storage in score data
    type Cache() =
        let charts, collections = Cache.Load()

        member this.Save() = Json.toFile(Path.Combine(getDataPath("Data"), "cache.json"), true)(charts, collections)
        static member Load() = loadImportantJsonFile("Cache")(Path.Combine(getDataPath("Data"), "cache.json"))((new Dictionary<string, CachedChart>(), new Dictionary<string, Collection>()))(false)
    
        member this.CacheChart (c: Chart) = lock(this) (fun () -> charts.[c.FileIdentifier] <- cacheChart c)

        member this.Count = charts.Count

        member this.LookupChart (id : string) : CachedChart option = if charts.ContainsKey(id) then Some charts.[id] else None

        member this.LoadChart (cc : CachedChart) : Chart option = 
            match cc.FilePath |> loadChartFile with
            | Some c ->
                this.CacheChart c
                Some c
            | None -> None

        member private this.FilterCharts(filter: string) =
            let filter = filter.ToLower()
            seq {
                for c in charts.Values do
                    if (c.Title + c.Artist + c.Creator + c.DiffName).ToLower().Contains(filter) then
                        yield c
            }

        member this.GetGroups grouping (sorting : Comparison<CachedChart>) filter =
            let groups = new Dictionary<string, List<CachedChart>>()
            for c in this.FilterCharts filter do
                let s = grouping(c)
                if (groups.ContainsKey(s) |> not) then groups.Add(s, new List<CachedChart>())
                groups.[s].Add(c)
            for g in groups.Values do
                g.Sort(sorting)
            groups

        member this.GetCollections (sorting : Comparison<CachedChart>) filter = 
            //todo: filter
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

        member this.RebuildCache : StatusTask =
            fun output ->
                async {
                    lock this (fun _ -> charts.Clear())
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
                    this.Save()
                    output("Saved cache.")
                    return true
                }

        member this.DeleteChart (c : CachedChart) = failwith "nyi"

        member this.DeleteCharts (cs : List<CachedChart>) = Seq.iter (this.DeleteChart) cs

        member this.ConvertSongFolder (path: string) (packname: string) : StatusTask =
            fun output ->
                async {
                    for file in Directory.EnumerateFiles(path) do
                        match file with
                        | ChartFile ->
                            output("Converting " + file)
                            try
                                loadAndConvertFile file
                                |> List.map (fun c -> relocateChart c path (Path.Combine(getDataPath "Songs", packname, Path.GetFileName(path))))
                                |> fun charts ->
                                    lock this (fun _ ->
                                    (
                                        List.iter this.CacheChart charts
                                    ))
                            with err -> Logging.Error("Failed to load/convert file: " + file)(err.ToString())
                        | _ -> ()
                    return true
                }

        member this.ConvertPackFolder (path: string) (packname: string) : StatusTask =
            fun output ->
                async {
                    let! _ =
                        Directory.EnumerateDirectories(path)
                        |> Seq.map (fun song -> (this.ConvertSongFolder song packname output))
                        |> fun s -> Async.Parallel(s, 5)
                    return true
                }

        member this.AutoConvert path : StatusTask =
            fun output ->
                async {
                    match File.GetAttributes(path) &&& FileAttributes.Directory |> int with
                    | 0 ->
                        match path with
                        | ChartFile -> failwith "single chart file not yet supported"
                        | ChartArchive ->
                            output("Extracting...")
                            let dir = Path.ChangeExtension(path, null)
                            //Logging.Debug("There is no check against possibly malicious directory traversal  (for now). Be careful")("")
                            ZipFile.ExtractToDirectory(path, dir)
                            output("Extracted! " + dir)
                            let! b = this.AutoConvert(dir)(output)
                            Directory.Delete(dir, true)
                        | _ -> failwith "unrecognised file"
                    | _ ->
                        match path with
                        | SongFolder ->
                            BackgroundTask.Create(TaskFlags.NONE)("Import " + Path.GetFileName(path))(this.ConvertSongFolder path "Singles")
                        | PackFolder ->
                            let packname =
                                match Path.GetFileName(path) with
                                | "Songs" -> if path |> Path.GetDirectoryName |> Path.GetFileName = "osu!" then "osu!" else "Songs"
                                | s -> s
                            BackgroundTask.Create(TaskFlags.NONE)("Import pack " + Path.GetFileName(path))(this.ConvertPackFolder path packname)
                        | FolderOfPacks ->
                            do! 
                                Directory.EnumerateDirectories(path)
                                |> Seq.map (fun packFolder -> this.ConvertPackFolder(packFolder)(Path.GetFileName(packFolder))(output))
                                |> fun s -> Async.Parallel(s, 5)
                                |> Async.Ignore
                        | _ -> failwith "no importable folder structure detected"
                    return true
                }