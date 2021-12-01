namespace Prelude.Data.Charts

open System
open System.IO
open System.IO.Compression
open System.Collections.Generic
open System.Collections.Concurrent
open Percyqaz.Json
open Prelude.Common
open Prelude.ChartFormats.Interlude
open Prelude.ChartFormats.Conversions
open Prelude.Scoring
open Prelude.Gameplay.Mods
open Prelude.Gameplay.Layout
open Prelude.Gameplay.Difficulty

module Caching =

    [<Json.AllRequired>]
    type CachedChart =
        {
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
        static member Default =
            {
                FilePath = ""
                Title = ""
                Artist = ""
                Creator = ""
                Pack = ""
                Hash = ""
                Keys = 4
                Length = 0.0f<ms>
                BPM = (500.0f<ms/beat>, 500.0f<ms/beat>)
                DiffName = ""
                Physical = 0.0
                Technical = 0.0
            }

    let cacheChart (chart: Chart) : CachedChart =
        let lastNote = chart.LastNote
        let rating = RatingReport(chart.Notes, 1.0f, Layout.Spread, chart.Keys)
        {
            FilePath = chart.FileIdentifier
            Title = chart.Header.Title
            Artist = chart.Header.Artist
            Creator = chart.Header.Creator
            Pack = chart.Header.SourcePack
            Hash = Chart.hash chart
            Keys = chart.Keys
            Length = lastNote - chart.FirstNote
            // todo: move to Chart module
            BPM = ``Interlude to osu!``.minMaxBPM (List.ofSeq chart.BPM.Data) lastNote
            DiffName = chart.Header.DiffName
            Physical = rating.Physical
            Technical = rating.Technical
        }

module Collections =

    type Goal =
    | NoGoal
    | Clear of Metrics.HPSystemConfig
    | Lamp of Metrics.AccuracySystemConfig * Lamp
    | Accuracy of Metrics.AccuracySystemConfig * float

    type PlaylistData = string * ModState * float32
    type Collection =
    | Collection of List<string>
    | Playlist of List<PlaylistData> //order of list matters
    | Goals of List<PlaylistData * Goal>
        member this.ToCollection() =
            match this with
            | Collection l -> Collection l
            | Playlist p -> p |> Seq.map (fun (i, _, _) -> i) |> List |> Collection
            | Goals g -> g |> Seq.map (fun ((i, _, _), _) -> i) |> List |> Collection
        member this.ToPlaylist(mods, rate) = 
            match this with
            | Collection l -> l |> Seq.map (fun i -> (i, mods, rate)) |> List |> Playlist
            | Playlist p -> Playlist p
            | Goals g -> g |> Seq.map (fun (x, _) -> x) |> List |> Playlist
        member this.IsEmpty() =
            match this with
            | Collection l -> l.Count = 0
            | Playlist p -> p.Count = 0
            | Goals g -> g.Count = 0
        static member Blank = Collection (ResizeArray<_>())

module Sorting =

    open Caching
    open FParsec

    let private firstCharacter (s: string) =
        if s.Length = 0 then "?"
        elif Char.IsLetterOrDigit s.[0] then s.[0].ToString().ToUpper()
        else "?"

    type GroupMethod = CachedChart -> string
    let groupBy : IDictionary<string, GroupMethod> = dict[
            "Physical", fun c -> let i = int (c.Physical / 2.0) * 2 in i.ToString().PadLeft(2, '0') + " - " + (i + 2).ToString().PadLeft(2, '0')
            "Technical", fun c -> let i = int (c.Technical / 2.0) * 2 in i.ToString().PadLeft(2, '0') + " - " + (i + 2).ToString().PadLeft(2, '0')
            "Pack", fun c -> c.Pack
            "Title", fun c -> firstCharacter c.Title
            "Artist", fun c -> firstCharacter c.Artist
            "Creator", fun c -> firstCharacter c.Creator
            "Keymode", fun c -> c.Keys.ToString() + "k"
            "Collections", fun _ ->  "" // Placeholder for UI purposes, UI is hard coded to call collection grouping behaviour when this is chosen
        ]

    type SortMethod = Comparison<CachedChart>
    let sortBy : IDictionary<string, SortMethod> = dict[
            "Physical", Comparison(fun a b -> a.Physical.CompareTo b.Physical)
            "Technical", Comparison(fun a b -> a.Technical.CompareTo b.Technical)
            "Title", Comparison(fun a b -> a.Title.CompareTo b.Title)
            "Artist", Comparison(fun a b -> a.Artist.CompareTo b.Artist)
            "Creator", Comparison(fun a b -> a.Creator.CompareTo b.Creator)
        ]

    type FilterPart = 
        | Criterion of string * string
        | String of string
        | Impossible
    type Filter = FilterPart list

    module Filter =

        let private string = " =:<>\"" |> isNoneOf |> many1Satisfy |>> fun s -> s.ToLower()
        let private word = string |>> String
        let private pstring = between (pchar '"') (pchar '"') ("\"" |> isNoneOf |> many1Satisfy) |>> fun s -> String <| s.ToLower()
        let private criterion = string .>>. (pchar '=' >>. string) |>> Criterion
        let private filter = sepBy (attempt criterion <|> pstring <|> word) spaces1 .>> spaces

        let parse (str: string) =
            match run filter (str.Trim()) with
            | Success (x, _, _) -> x
            | Failure (f, _, _) -> [Impossible]

        let apply (filter: Filter) (charts: CachedChart seq) =
            seq {
                for c in charts do
                    let s = (c.Title + " " + c.Artist + " " + c.Creator + " " + c.DiffName + " " + c.Pack).ToLower()
                    if List.forall
                        (
                            function
                            | Impossible -> false
                            | String str -> s.Contains str
                            | Criterion ("k", n)
                            | Criterion ("key", n)
                            | Criterion ("keys", n) -> c.Keys.ToString() = n
                            | _ -> true
                        )
                        filter
                    then yield c
            }
            
open Sorting
open Collections
open Caching

//todo: add reverse lookup from hash -> id for score -> chart lookup
module Library =

    do
        fun (cache, settings, rules) ->
            Json.Mapping.getCodec<Dictionary<string, CachedChart>>(cache, settings, rules)
            |> Json.Mapping.Codec.map
                (fun (d: ConcurrentDictionary<string, CachedChart>) -> Dictionary(d))
                (fun (d: Dictionary<string, CachedChart>) -> ConcurrentDictionary(d))
        |> Json.Mapping.Rules.typeRule<ConcurrentDictionary<string, CachedChart>>
        |> JSON.AddRule

    type Data =
        {
            Charts: ConcurrentDictionary<string, CachedChart>
            Collections: Dictionary<string, Collection>
        }
        static member Default = { Charts = new ConcurrentDictionary<string, CachedChart>(); Collections = new Dictionary<string, Collection>() }

    let data =
        loadImportantJsonFile "Cache" (Path.Combine(getDataPath "Data", "cache.json")) Data.Default false
        |> fun d -> Logging.Info (sprintf "Cache loaded, %i charts and %i collections." d.Charts.Keys.Count d.Collections.Keys.Count); d
    let charts, collections = data.Charts, data.Collections

    // ----

    let save() = JSON.ToFile(Path.Combine(getDataPath "Data", "cache.json"), true) data
    
    let addOrUpdate (c: Chart) = charts.[c.FileIdentifier] <- cacheChart c

    let count() = charts.Count

    let lookup (id: string) : CachedChart option =
        let t, c = charts.TryGetValue id
        if t then Some c else None

    let load (cc: CachedChart) : Chart Option =
        cc.FilePath |> Chart.fromFile
        (*|> function | Some c -> addOrUpdate c; Some c | None -> None*)

    module Collections =
        
        let getNewName () =
            let mutable name = "New Collection"
            let mutable n = 0
            while collections.ContainsKey name do
                n <- n + 1
                name <- "New Collection " + n.ToString()
            name

        /// Returns false only in thread-unsafe case when two threads both make a new collection with same name
        let create (name: string, data: Collection) : bool =
            collections.TryAdd(name, data)

        let rename (id: string, newId: string) : bool =
            if collections.ContainsKey newId then false else
                collections.Add (newId, collections.[id])
                collections.Remove id

        let get (id: string) : Collection option = if collections.ContainsKey id then Some collections.[id] else None

        let update (id: string, data: Collection) =
            if collections.ContainsKey id then collections.[id] <- data else failwith "No such collection."

        let delete (id: string) : bool = if collections.ContainsKey id then collections.Remove id else false

        let enumerate () = collections.Keys :> string seq

    let getGroups (grouping: GroupMethod) (sorting: SortMethod) (filter: Filter) =
        let groups = new Dictionary<string, ResizeArray<CachedChart>>()
        for c in Filter.apply filter charts.Values do
            let s = grouping c
            if groups.ContainsKey s |> not then groups.Add(s, new ResizeArray<CachedChart>())
            groups.[s].Add c
        for g in groups.Values do
            g.Sort sorting
        groups

    let getCollectionGroups (sorting: SortMethod) (filter: Filter) =
        let groups = new Dictionary<string, ResizeArray<CachedChart>>()
        for name in collections.Keys do
            let c = collections.[name]
            let charts, orderMatters = 
                match c with
                | Collection ids -> Seq.choose lookup ids, false
                | Playlist ps -> Seq.choose (fun (i, _, _) -> lookup i) ps, true
                | Goals gs -> Seq.choose (fun ((i, _, _), _) -> lookup i) gs, false
            charts
            |> Filter.apply filter
            |> ResizeArray<CachedChart>
            |> if orderMatters then id else fun x -> x.Sort sorting; x
            |> fun x -> if x.Count > 0 then groups.Add(name, x)
        groups

    let rebuildTask : StatusTask =
        fun output ->
            async {
                charts.Clear()
                for pack in Directory.EnumerateDirectories(getDataPath "Songs") do
                    for song in Directory.EnumerateDirectories pack do
                        for file in Directory.EnumerateFiles song do
                            match Path.GetExtension(file).ToLower() with
                            | ".yav" ->
                                match Chart.fromFile file with
                                | Some c ->
                                    output ("Caching " + file)
                                    addOrUpdate c
                                | None -> ()
                            | _ -> ()
                save()
                output "Saved cache."
                return true
            }

    let delete (c: CachedChart) =
        charts.TryRemove c.FilePath |> ignore
        if File.Exists c.FilePath then File.Delete c.FilePath
        let songfolder = Path.GetDirectoryName c.FilePath
        match songfolder with SongFolder _ -> () | _ -> Directory.Delete(songfolder, true) 
        let packfolder = Path.GetDirectoryName songfolder
        if packfolder |> Directory.EnumerateDirectories |> Seq.isEmpty then Directory.Delete(packfolder, false)

    let deleteMany (cs: CachedChart seq) = Seq.iter delete cs

    module Imports =
    
        let osuSongFolder = Path.Combine (Environment.GetFolderPath Environment.SpecialFolder.LocalApplicationData, "osu!", "Songs")
        let stepmaniaPackFolder = Path.Combine (Path.GetPathRoot Environment.CurrentDirectory, "Games", "Stepmania 5", "Songs")
        let etternaPackFolder = Path.Combine (Path.GetPathRoot Environment.CurrentDirectory, "Games", "Etterna", "Songs")

        type MountedChartSourceType =
            | Pack of name: string
            | Library
        type MountedChartSource =
            {
                SourceFolder: string
                mutable LastImported: DateTime
                Type: MountedChartSourceType
                ImportOnStartup: bool
            }
            static member Pack (name: string, path: string) =
                { SourceFolder = path; LastImported = DateTime.UnixEpoch; Type = Pack name; ImportOnStartup = false }
            static member Library (path: string) =
                { SourceFolder = path; LastImported = DateTime.UnixEpoch; Type = Library; ImportOnStartup = false }

        let convertSongFolder (path: string) (config: ConversionActionConfig) : StatusTask =
            fun output ->
                async {
                    for file in Directory.EnumerateFiles path do
                        match file with
                        | ChartFile _ ->
                            output ("Converting " + file)
                            try
                                let action = { Config = config; Source = file; TargetDirectory = Path.Combine (getDataPath "Songs", config.PackName, Path.GetFileName path) }
                                loadAndConvertFile action
                                |> List.map (relocateChart action)
                                |> fun charts -> List.iter addOrUpdate charts
                            with err -> Logging.Error ("Failed to load/convert file: " + file, err)
                        | _ -> ()
                    return true
                }

        let convertPackFolder (path: string) (config: ConversionActionConfig) : StatusTask =
            fun output ->
                async {
                    let! _ =
                        Directory.EnumerateDirectories path
                        |> match config.ChangedAfter with None -> id | Some timestamp -> Seq.filter (fun path -> Directory.GetLastWriteTime path >= timestamp)
                        |> Seq.map (fun song -> (convertSongFolder song config output))
                        |> fun s -> Async.Parallel(s, 5)
                    return true
                }

        let importMountedSource (source: MountedChartSource) : StatusTask =
            fun output ->
                async {
                    match source.Type with
                    | Pack packname ->
                        let config = { ConversionActionConfig.Default with CopyMediaFiles = false; ChangedAfter = Some source.LastImported; PackName = packname }
                        let! res = convertPackFolder source.SourceFolder config output
                        if res then source.LastImported <- DateTime.Now
                        return res
                    | Library ->
                        do! 
                            Directory.EnumerateDirectories source.SourceFolder
                            |> Seq.filter (fun path -> Directory.GetLastWriteTime path >= source.LastImported)
                            |> Seq.map (fun packFolder -> convertPackFolder packFolder { ConversionActionConfig.Default with CopyMediaFiles = false; ChangedAfter = Some source.LastImported; PackName = Path.GetFileName packFolder } output)
                            |> fun s -> Async.Parallel(s, 5)
                            |> Async.Ignore
                        source.LastImported <- DateTime.Now
                        return true
                }

        let rec autoConvert path : StatusTask =
            fun output ->
                async {
                    match File.GetAttributes path &&& FileAttributes.Directory |> int with
                    | 0 ->
                        match path with
                        | ChartFile ext ->
                            return!
                                convertSongFolder (Path.GetDirectoryName path)
                                    { ConversionActionConfig.Default with PackName = if ext = ".osu" then "osu!" else "Singles" }
                                    output
                        | ChartArchive ->
                            output "Extracting..."
                            let dir = Path.ChangeExtension(path, null)
                            // Note that this method has built-in directory traversal protection
                            ZipFile.ExtractToDirectory(path, dir)
                            output ("Extracted! " + dir)
                            return! 
                                (
                                    autoConvert dir
                                    |> BackgroundTask.Callback(fun _ -> output "Deleting extracted directory."; Directory.Delete(dir, true))
                                ) output
                        | _ -> return failwith "Unrecognised file"
                    | _ ->
                        match path with
                        | SongFolder ext ->
                            return! convertSongFolder path { ConversionActionConfig.Default with PackName = if ext = ".osu" then "osu!" else "Singles" } output
                        | PackFolder ->
                            let packname =
                                match Path.GetFileName path with
                                | "Songs" -> if path |> Path.GetDirectoryName |> Path.GetFileName = "osu!" then "osu!" else "Songs"
                                | s -> s
                            return! (convertPackFolder path { ConversionActionConfig.Default with PackName = packname }) output
                        | FolderOfPacks ->
                            do! 
                                Directory.EnumerateDirectories path
                                |> Seq.map (fun packFolder -> convertPackFolder packFolder { ConversionActionConfig.Default with PackName = Path.GetFileName packFolder } output)
                                |> fun s -> Async.Parallel(s, 5)
                                |> Async.Ignore
                            return true
                        | _ -> return failwith "No importable folder structure detected"
                }