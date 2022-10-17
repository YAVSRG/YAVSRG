namespace Prelude.Data.Charts

open System
open System.IO
open System.IO.Compression
open System.Collections.Generic
open System.Collections.Concurrent
open Percyqaz.Json
open Percyqaz.Common
open Prelude.Common
open Prelude.Charts.Formats.Interlude
open Prelude.Charts.Formats.Conversions
open Prelude.Data.Charts.Tables

open Sorting
open Collections
open Caching

module Library =

    let charts : ConcurrentDictionary<string, CachedChart> = loadImportantJsonFile "Cache" (Path.Combine(getDataPath "Data", "cache.json")) false
    let collections = 
        let cs : Dictionary<string, Collection> = loadImportantJsonFile "Cache" (Path.Combine(getDataPath "Data", "collections.json")) false
        Logging.Info (sprintf "Loaded chart library of %i charts, %i collections" charts.Keys.Count cs.Keys.Count)
        cs

    // ---- Basic data layer stuff ----

    let save() = 
        saveImportantJsonFile (Path.Combine(getDataPath "Data", "cache.json")) charts
        saveImportantJsonFile (Path.Combine(getDataPath "Data", "collections.json")) collections
    
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

    let recache_service =
        { new Async.Service<unit, unit>() with
            override this.Handle(()) =
                async {
                    Logging.Debug("Rebuilding chart cache")
                    charts.Clear()
                    for pack in Directory.EnumerateDirectories(getDataPath "Songs") do
                        for song in Directory.EnumerateDirectories pack do
                            for file in Directory.EnumerateFiles song do
                                match Path.GetExtension(file).ToLower() with
                                | ".yav" ->
                                    match Chart.fromFile file with
                                    | Some c ->
                                        addOrUpdate c
                                    | None -> ()
                                | _ -> ()
                    save()
                }
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
        
        [<Json.AutoCodec>]
        type MountedChartSourceType =
            | Pack of name: string
            | Library
        
        [<Json.AutoCodec>]
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

        let convert_song_folder = 
            { new Async.Service<string * ConversionActionConfig, unit>() with
                override this.Handle((path, config)) =
                    async {
                        for file in Directory.EnumerateFiles path do
                            match file with
                            | ChartFile _ ->
                                try
                                    let action = { Config = config; Source = file; TargetDirectory = Path.Combine (getDataPath "Songs", config.PackName, Path.GetFileName path) }
                                    loadAndConvertFile action
                                    |> List.map (relocateChart action)
                                    |> fun charts -> List.iter addOrUpdate charts
                                with err -> Logging.Error ("Failed to load/convert file: " + file, err)
                            | _ -> ()
                    }
            }

        let convert_pack_folder =
            { new Async.Service<string * ConversionActionConfig, unit>() with
                override this.Handle((path, config)) =
                    async {
                        for songFolder in
                            Directory.EnumerateDirectories path
                            |> match config.ChangedAfter with None -> id | Some timestamp -> Seq.filter (fun path -> Directory.GetLastWriteTime path >= timestamp)
                            do
                            do! convert_song_folder.RequestAsync(songFolder, config)
                    }
            }

        let import_mounted_source =
            
            { new Async.Service<MountedChartSource, unit>() with
                override this.Handle(source) =
                    async {
                        match source.Type with
                        | Pack packname ->
                            let config = { ConversionActionConfig.Default with CopyMediaFiles = false; ChangedAfter = Some source.LastImported; PackName = packname }
                            do! convert_pack_folder.RequestAsync(source.SourceFolder, config)
                            source.LastImported <- DateTime.Now
                        | Library ->
                            for packFolder in
                                Directory.EnumerateDirectories source.SourceFolder
                                |> Seq.filter (fun path -> Directory.GetLastWriteTime path >= source.LastImported)
                                do
                                do! convert_pack_folder.RequestAsync(packFolder, { ConversionActionConfig.Default with CopyMediaFiles = false; ChangedAfter = Some source.LastImported; PackName = Path.GetFileName packFolder })
                            source.LastImported <- DateTime.Now
                    }
            }

        let auto_convert =
            { new Async.Service<string, bool>() with
                override this.Handle(path) =
                    async {
                        match File.GetAttributes path &&& FileAttributes.Directory |> int with
                        | 0 ->
                            match path with
                            | ChartFile ext ->
                                do! convert_song_folder.RequestAsync(
                                    Path.GetDirectoryName path,
                                    { ConversionActionConfig.Default with PackName = if ext = ".osu" then "osu!" else "Singles" })
                                return true
                            | ChartArchive ->
                                let dir = Path.ChangeExtension(path, null)
                                // This has built-in directory traversal protection
                                ZipFile.ExtractToDirectory(path, dir)
                                this.Request(dir, fun _ -> Directory.Delete(dir, true))
                                return true
                            | _ -> Logging.Warn(sprintf "%s: Unrecognised file for import" path); return false
                        | _ ->
                            match path with
                            | SongFolder ext ->
                                do! convert_song_folder.RequestAsync(path, { ConversionActionConfig.Default with PackName = if ext = ".osu" then "osu!" else "Singles" })
                                return true
                            | PackFolder ->
                                let packname =
                                    match Path.GetFileName path with
                                    | "Songs" -> if path |> Path.GetDirectoryName |> Path.GetFileName = "osu!" then "osu!" else "Songs"
                                    | s -> s
                                do! convert_pack_folder.RequestAsync(path, { ConversionActionConfig.Default with PackName = packname })
                                return true
                            | FolderOfPacks ->
                                for packFolder in Directory.EnumerateDirectories path do
                                    do! convert_pack_folder.RequestAsync(packFolder, { ConversionActionConfig.Default with PackName = Path.GetFileName packFolder })
                                return true
                            | _ -> Logging.Warn(sprintf "%s: No importable folder structure detected" path); return false
                    }
            }