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
open Prelude.Data.Tables

open Sorting
open Collections
open Caching

module Library =

    do
        fun (cache, settings, rules) ->
            Json.Mapping.getCodec<Dictionary<string, CachedChart>>(cache, settings, rules)
            |> Json.Mapping.Codec.map
                (fun (d: ConcurrentDictionary<string, CachedChart>) -> Dictionary d)
                (fun (d: Dictionary<string, CachedChart>) -> ConcurrentDictionary d)
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

    // ---- Basic data layer stuff ----

    let save() = saveImportantJsonFile (Path.Combine(getDataPath "Data", "cache.json")) data
    
    let addOrUpdate (c: Chart) = charts.[c.FileIdentifier] <- cacheChart c

    let count() = charts.Count

    let lookup (id: string) : CachedChart option =
        let success, c = charts.TryGetValue id
        if success then Some c else None

    // todo: optimise!
    let lookupHash(id: string) : CachedChart option =
        Seq.tryFind (fun cc -> cc.Hash = id) charts.Values

    let load (cc: CachedChart) : Chart Option =
        cc.FilePath |> Chart.fromFile
        (*|> function | Some c -> addOrUpdate c; Some c | None -> None*)

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
                                    output ("Caching " + Path.GetFileName file)
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

    // ---- Collections stuff ----

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

        let exists (id: string) : bool = collections.ContainsKey id

        let get (id: string) : Collection option = if collections.ContainsKey id then Some collections.[id] else None

        let update (id: string, data: Collection) =
            if collections.ContainsKey id then collections.[id] <- data else failwith "No such collection."

        let delete (id: string) : bool = if collections.ContainsKey id then collections.Remove id else false

        let enumerate () = collections.Keys :> string seq

        /// Returns the new index if successful
        let reorderPlaylist (id: string) (index: int) (up: bool) : int option =
            if collections.ContainsKey id then
                match collections.[id] with
                | Playlist ps ->
                    let newIndex = if up then max 0 (index - 1) else min (ps.Count - 1) (index + 1)
                    let item = ps.[index]
                    ps.RemoveAt index
                    ps.Insert (newIndex, item)
                    Some newIndex
                | _ -> None
            else None

    // ---- Retrieving library for level select ----

    type Group = ResizeArray<CachedChart * LevelSelectContext>
    type LexSortedGroups = Dictionary<int * string, Group>

    let getGroups (ctx: GroupContext) (grouping: GroupMethod) (sorting: SortMethod) (filter: Filter) : LexSortedGroups =
        let groups = new Dictionary<int * string, Group>()
        for c in Filter.apply filter charts.Values do
            let s = grouping (c, ctx)
            if groups.ContainsKey s |> not then groups.Add(s, new ResizeArray<CachedChart * LevelSelectContext>())
            groups.[s].Add (c, LevelSelectContext.None)
        for g in groups.Values do
            g.Sort sorting
        groups

    let getCollectionGroups (sorting: SortMethod) (filter: Filter) : LexSortedGroups =
        let groups = new Dictionary<int * string, Group>()
        for name in collections.Keys do
            let c = collections.[name]
            let charts, orderMatters = 
                match c with
                | Collection ids -> 
                    Seq.choose
                        ( fun (index, id) ->
                            lookup id
                            |> Option.map (fun x -> x, LevelSelectContext.Collection (index, name))
                        )
                        (Seq.indexed ids),
                    false
                | Playlist ps ->
                    Seq.choose
                        ( fun (index, (i, data)) -> 
                            lookup i
                            |> Option.map (fun x -> x, LevelSelectContext.Playlist (index, name, data))
                        ) 
                        (Seq.indexed ps),
                    true
                | Goals gs -> 
                    Seq.choose
                        ( fun (index, (i, data)) -> 
                            lookup i
                            |> Option.map (fun x -> x, LevelSelectContext.Goal (index, name, data))
                        ) 
                        (Seq.indexed gs),
                    false
            charts
            |> Filter.applyf filter
            |> ResizeArray<CachedChart * LevelSelectContext>
            |> if orderMatters then id else fun x -> x.Sort sorting; x
            |> fun x -> if x.Count > 0 then groups.Add((0, name), x)
        groups

    let getTableGroups (sorting: SortMethod) (filter: Filter) : LexSortedGroups =
        let groups = new Dictionary<int * string, Group>()
        match Table.current with
        | Some table ->
            for level_no, level in Seq.indexed table.Levels do
                let charts =
                    Seq.choose
                        ( fun (c: TableChart) ->
                            lookupHash c.Hash
                            |> Option.map (fun x -> x, LevelSelectContext.Table)
                        ) level.Charts
                charts
                |> Filter.applyf filter
                |> ResizeArray<CachedChart * LevelSelectContext>
                |> fun x -> x.Sort sorting; x
                |> fun x -> if x.Count > 0 then groups.Add((level_no, level.Name), x)
        | None -> ()
        groups

    // ---- Importing charts to library ----

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
                            output ("Converting " + Path.GetFileName file)
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