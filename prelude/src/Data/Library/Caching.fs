﻿namespace Prelude.Data.Library.Caching

open System
open System.IO
open System.Collections.Generic
open System.Collections.Concurrent
open System.Security.Cryptography
open Percyqaz.Common
open Percyqaz.Data
open Prelude
open Prelude.Charts
open Prelude.Charts.Processing.Difficulty
open Prelude.Charts.Processing.Patterns

[<Json.AutoCodec(true)>]
type CachedChart =
    {
        Artist: string
        Title: string
        Subtitle: string option
        Source: string option
        Creator: string
        Tags: string list
        Folder: string
        Hash: string
        Keys: int
        Length: Time
        DateAdded: DateTime
        BPM: (float32<ms / beat> * float32<ms / beat>)
        DifficultyName: string
        Physical: float
        BackgroundFile: string option
        AudioFile: string option
    }
    // todo: this exists for performance gain reasons. change to an sqlite database instead
    static member Default =
        {
            Artist = ""
            Title = ""
            Subtitle = None
            Source = None
            Creator = ""
            Tags = []
            Folder = ""
            Hash = ""
            Keys = 4
            Length = 0.0f<ms>
            DateAdded = Unchecked.defaultof<_>
            BPM = (0.0f<ms / beat>, 0.0f<ms / beat>)
            DifficultyName = ""
            Physical = 0.0
            BackgroundFile = None
            AudioFile = None
        }

    member this.Key = sprintf "%s/%s" this.Folder this.Hash

type Cache =
    {
        RootPath: string
        Entries: ConcurrentDictionary<string, CachedChart>
        Patterns: ConcurrentDictionary<string, PatternInfo>
        mutable Changed: bool
    }

module Cache =

    (* IO operations *)

    let from_path (path: string) =
        Directory.CreateDirectory path |> ignore

        File.WriteAllText(
            Path.Combine(path, "HOW_TO_ADD_SONGS.txt"),
            "Dragging and dropping things into this folder won't work.\n"
            + "Instead, drag and drop things onto the *game window* while it's open and it will import.\n\n"
            + "Does this folder have stuff in, but they don't show up in game?\n"
            + "In that case, check they are .yav files, and then go to Options > Debug > Rebuild cache and let that run."
        )

        {
            RootPath = path
            Entries = load_important_json_file "Cache" (Path.Combine(path, "cache.json")) false
            Patterns =
                let path = Path.Combine(path, "patterns.json")
                match JSON.FromFile path with
                | Ok res -> res
                | Error reason ->
                    ConcurrentDictionary<string, PatternInfo>()
            Changed = false
        }

    let save (cache: Cache) =
        if cache.Changed then
            save_important_json_file (Path.Combine(cache.RootPath, "cache.json")) cache.Entries
            JSON.ToFile (Path.Combine(cache.RootPath, "patterns.json"), true) cache.Patterns
            cache.Changed <- false

    let private create_entry (folder_name: string) (file_time: DateTime) (chart: Chart) : CachedChart * PatternInfo =
        let last_note = chart.LastNote
        let rating = DifficultyRating.calculate 1.0f chart.Notes

        {
            Artist = chart.Header.Artist
            Title = chart.Header.Title
            Subtitle = chart.Header.Subtitle
            Source = chart.Header.Source
            Creator = chart.Header.Creator
            Tags = chart.Header.Tags
            Folder = folder_name
            Hash = Chart.hash chart
            Keys = chart.Keys
            Length = last_note - chart.FirstNote
            DateAdded = file_time
            BPM = Chart.find_min_max_bpm chart
            DifficultyName = chart.Header.DiffName
            Physical = rating.Physical
            BackgroundFile =
                match chart.Header.BackgroundFile with
                | Asset s -> Some s
                | _ -> None
            AudioFile =
                match chart.Header.AudioFile with
                | Asset s -> Some s
                | _ -> None
        },
        PatternSummary.generate_pattern_data 1.0f chart

    let private sha_256 = SHA256.Create()

    let compute_hash (file_path: string) : string =
        use stream = File.OpenRead file_path

        sha_256.ComputeHash stream
        |> BitConverter.ToString
        |> fun s -> s.Replace("-", "")

    let hash_asset (file_path: string) (cache: Cache) : string =
        if not (File.Exists file_path) then
            failwithf "Missing asset file: %s" file_path

        let hash = compute_hash file_path
        let target_folder = Path.Combine(cache.RootPath, ".assets", hash.Substring(0, 2))
        Directory.CreateDirectory target_folder |> ignore
        let target_path = Path.Combine(target_folder, hash)

        if File.Exists target_path then
            File.Delete file_path
        else
            File.Move(file_path, target_path)

        hash

    let asset_path (hash: string) (cache: Cache) : string =
        Path.Combine(cache.RootPath, ".assets", hash.Substring(0, 2), hash)

    (* Using the cache *)

    let get_path (entry: CachedChart) (cache: Cache) =
        Path.Combine(cache.RootPath, entry.Folder, entry.Hash + ".yav")

    let background_path (chart: Chart) (cache: Cache) : string option =
        match chart.Header.BackgroundFile with
        | Relative s -> Path.Combine(Path.GetDirectoryName chart.LoadedFromPath, s) |> Some
        | Absolute s -> Some s
        | Asset s -> Path.Combine(cache.RootPath, ".assets", s.Substring(0, 2), s) |> Some
        | Missing -> None

    let audio_path (chart: Chart) (cache: Cache) =
        match chart.Header.AudioFile with
        | Relative s -> Path.Combine(Path.GetDirectoryName chart.LoadedFromPath, s) |> Some
        | Absolute s -> Some s
        | Asset s -> Path.Combine(cache.RootPath, ".assets", s.Substring(0, 2), s) |> Some
        | Missing -> None

    /// Behold the beauty of this codebase
    /// Yes Fantomas keeps it in this format and it was originally written like this
    let recache_service =
        { new Async.Service<Cache, unit>() with
            override this.Handle(cache: Cache) =
                async {
                    Logging.Debug(sprintf "Rebuilding chart cache @ %s" cache.RootPath)
                    cache.Entries.Clear()
                    cache.Changed <- true

                    for folder in
                        Directory.EnumerateDirectories cache.RootPath
                        |> Seq.filter (fun p -> Path.GetFileName p <> ".assets") do

                        // backwards compatible stuff. move all loose folders
                        for song in Directory.EnumerateDirectories folder do
                            let moved_assets = Dictionary<string, string>() // relative file name -> hash

                            for file in Directory.EnumerateFiles song do
                                match Path.GetExtension(file).ToLower() with
                                | ".yav" ->
                                    match Chart.from_file file with
                                    | Some c ->
                                        try
                                            let c =
                                                { c with
                                                    Header =
                                                        { c.Header with
                                                            BackgroundFile =
                                                                match c.Header.BackgroundFile with
                                                                | Relative s ->
                                                                    if moved_assets.ContainsKey s then
                                                                        Asset moved_assets.[s]
                                                                    else
                                                                        let path = (background_path c cache).Value

                                                                        if not (File.Exists path) then
                                                                            Missing
                                                                        else
                                                                            let hash = hash_asset path cache
                                                                            moved_assets.[s] <- hash
                                                                            Asset hash
                                                                | Absolute s -> Absolute s
                                                                | Asset s -> Asset s
                                                                | Missing -> Missing

                                                            AudioFile =
                                                                match c.Header.AudioFile with
                                                                | Relative s ->
                                                                    if moved_assets.ContainsKey s then
                                                                        Asset moved_assets.[s]
                                                                    else
                                                                        let path = (audio_path c cache).Value

                                                                        if not (File.Exists path) then
                                                                            Missing
                                                                        else
                                                                            let hash = hash_asset path cache
                                                                            moved_assets.[s] <- hash
                                                                            Asset hash
                                                                | Absolute s -> Absolute s
                                                                | Asset s -> Asset s
                                                                | Missing -> Missing
                                                        }
                                                }

                                            let entry, patterns =
                                                create_entry (Path.GetFileName folder) (File.GetCreationTimeUtc file) c

                                            let new_file = get_path entry cache
                                            Chart.to_file c new_file
                                            File.SetCreationTimeUtc(new_file, entry.DateAdded)
                                            File.Delete file

                                            cache.Entries.[entry.Key] <- entry
                                            cache.Patterns.[entry.Hash] <- patterns
                                            cache.Changed <- true
                                        with err ->
                                            Logging.Error(
                                                sprintf "Error (likely assets) with caching a legacy yav file: %s" file,
                                                err
                                            )
                                    | None -> ()
                                | _ -> ()

                            try
                                Directory.Delete song
                            with err ->
                                Logging.Warn(sprintf "Error deleting should-be-empty folder: %s" song, err)

                        // current system. just very minor sanity checks then straight into cache
                        for file in Directory.EnumerateFiles folder do
                            match Path.GetExtension(file).ToLower() with
                            | ".yav" ->
                                match Chart.from_file file with
                                | Some c ->
                                    let entry, patterns = create_entry (Path.GetFileName folder) (File.GetCreationTimeUtc file) c

                                    match c.Header.BackgroundFile with
                                    | Relative _ ->
                                        Logging.Warn(
                                            sprintf
                                                ".yav files in the cache (%s) shouldn't have relative assets, this is a legacy feature"
                                                file
                                        )
                                    | _ -> ()

                                    match c.Header.AudioFile with
                                    | Relative _ ->
                                        Logging.Warn(
                                            sprintf
                                                ".yav files in the cache (%s) shouldn't have relative assets, this is a legacy feature"
                                                file
                                        )
                                    | _ -> ()

                                    let intended_path = get_path entry cache

                                    if intended_path <> file && File.Exists intended_path then
                                        Logging.Warn(
                                            sprintf
                                                "Deleting %s because it should be named %s, which already exists"
                                                file
                                                intended_path
                                        )

                                        File.Delete file
                                    else
                                        if intended_path <> file then
                                            Logging.Debug(sprintf "Moving cache file from %s to %s" file intended_path)
                                            File.Move(file, intended_path)

                                        cache.Entries.[entry.Key] <- entry
                                        cache.Patterns.[entry.Hash] <- patterns
                                        cache.Changed <- true
                                | None -> ()
                            | _ -> ()

                    save cache
                }
        }

    // For charts being imported from someplace outside of any cache
    let add_new (folder: string) (charts: Chart list) (cache: Cache) =
        let moved_assets = Dictionary<string, string>()
        let now = DateTime.UtcNow

        for c in charts do
            let c =
                { c with
                    Header =
                        { c.Header with
                            BackgroundFile =
                                match c.Header.BackgroundFile with
                                | Relative s ->
                                    if moved_assets.ContainsKey s then
                                        Asset moved_assets.[s]
                                    else
                                        let path = (background_path c cache).Value

                                        if not (File.Exists path) then
                                            Missing
                                        else
                                            let hash = hash_asset path cache
                                            moved_assets.[s] <- hash
                                            Asset hash
                                | Absolute s -> Absolute s
                                | Asset s -> Asset s
                                | Missing -> Missing

                            AudioFile =
                                match c.Header.AudioFile with
                                | Relative s ->
                                    if moved_assets.ContainsKey s then
                                        Asset moved_assets.[s]
                                    else
                                        let path = (audio_path c cache).Value

                                        if not (File.Exists path) then
                                            Missing
                                        else
                                            let hash = hash_asset path cache
                                            moved_assets.[s] <- hash
                                            Asset hash
                                | Absolute s -> Absolute s
                                | Asset s -> Asset s
                                | Missing -> Missing
                        }
                }

            let entry, patterns = create_entry folder now c
            let new_file = get_path entry cache
            Directory.CreateDirectory(Path.GetDirectoryName new_file) |> ignore
            Chart.to_file c new_file

            cache.Entries.[entry.Key] <- entry
            cache.Patterns.[entry.Hash] <- patterns
            cache.Changed <- true

    // For copying a chart from one folder to another
    let copy (folder: string) (entry: CachedChart) (cache: Cache) =
        let new_entry = { entry with Folder = folder }

        if cache.Entries.ContainsKey(new_entry.Key) then
            ()
        else

            cache.Entries.[new_entry.Key] <- new_entry
            cache.Changed <- true
            let target = get_path new_entry cache
            Directory.CreateDirectory(Path.GetDirectoryName target) |> ignore
            File.Copy(get_path entry cache, target)

    // For charts being copied from one cache to another
    let replicate (folder: string) (entry: CachedChart) (source: Cache) (target: Cache) =
        if target.Entries.ContainsKey({ entry with Folder = folder }.Key) then
            ()
        else
            let chart = (Chart.from_file (get_path entry source)).Value

            let chart =
                { chart with
                    Header =
                        { chart.Header with
                            BackgroundFile =
                                match chart.Header.BackgroundFile with
                                | Relative _ ->
                                    let path = (background_path chart source).Value

                                    if not (File.Exists path) then
                                        Missing
                                    else
                                        let hash = hash_asset path target
                                        Asset hash
                                | Asset s ->
                                    let source = Path.Combine(source.RootPath, ".assets", s.Substring(0, 2), s)
                                    let target = Path.Combine(target.RootPath, ".assets", s.Substring(0, 2), s)

                                    if File.Exists target then
                                        ()
                                    else
                                        Directory.CreateDirectory(Path.GetDirectoryName target) |> ignore
                                        File.Copy(source, target)

                                    Asset s
                                | Absolute s -> Absolute s
                                | Missing -> Missing

                            AudioFile =
                                match chart.Header.AudioFile with
                                | Relative _ ->
                                    let path = (audio_path chart source).Value

                                    if not (File.Exists path) then
                                        Missing
                                    else
                                        let hash = hash_asset path target
                                        Asset hash
                                | Asset s ->
                                    let source = Path.Combine(source.RootPath, ".assets", s.Substring(0, 2), s)
                                    let target = Path.Combine(target.RootPath, ".assets", s.Substring(0, 2), s)

                                    if File.Exists target then
                                        ()
                                    else
                                        Directory.CreateDirectory(Path.GetDirectoryName target) |> ignore
                                        File.Copy(source, target)

                                    Asset s
                                | Absolute s -> Absolute s
                                | Missing -> Missing
                        }
                }

            let entry, patterns = create_entry folder DateTime.UtcNow chart
            let target_path = get_path entry target
            Directory.CreateDirectory(Path.GetDirectoryName target_path) |> ignore
            Chart.to_file chart target_path

            target.Entries.[entry.Key] <- entry
            target.Patterns.[entry.Hash] <- patterns
            target.Changed <- true

    let load (entry: CachedChart) (cache: Cache) = get_path entry cache |> Chart.from_file

    let by_key (key: string) (cache: Cache) : CachedChart option =
        let success, c = cache.Entries.TryGetValue key
        if success then Some c else None

    let by_hash (id: string) (cache: Cache) : CachedChart option =
        Seq.tryFind (fun cc -> cc.Hash = id) cache.Entries.Values

    let patterns_by_hash (id: string) (cache: Cache) : PatternInfo option =
        let success, p = cache.Patterns.TryGetValue id
        if success then Some p else None

    let delete (cc: CachedChart) (cache: Cache) =
        if cache.Entries.TryRemove (KeyValuePair(cc.Key, cc)) then 
            cache.Patterns.TryRemove cc.Hash |> ignore
            cache.Changed <- true
        let path = get_path cc cache

        if File.Exists path then
            File.Delete path
    // todo: remove assets IF they aren't used by anything else in the cache

    let delete_many (cs: CachedChart seq) (cache: Cache) = Seq.iter (fun c -> delete c cache) cs

    // Pattern stuff

    let cache_patterns =
        { new Async.Service<Cache * bool, unit>() with
            override this.Handle((cache, force)) =
                async {
                    if force then 
                        cache.Patterns.Clear()
                        cache.Changed <- true
                    for entry in cache.Entries.Values do
                        if not (cache.Patterns.ContainsKey entry.Hash) then
                            match load entry cache with
                            | Some chart ->
                                cache.Patterns.[entry.Hash] <- PatternSummary.generate_pattern_data 1.0f chart
                                cache.Changed <- true
                            | None -> ()
                }
        }

    let cache_patterns_if_needed (cache: Cache) (recache_complete_callback: unit -> unit) : bool =
        if cache.Entries.Count > 0 && cache.Patterns.Count = 0 then
            cache_patterns.Request((cache, true), recache_complete_callback)
            true
        else false

    // Download protocol

    open Prelude.Backbeat.Archive
    open Prelude.Data.WebServices
    open System.Net.Http

    let private httpclient = new HttpClient()

    let cdn_download (folder: string) (hash: string) (chart: Chart, song: Song) (cache: Cache) =
        async {
            let header = Archive.make_chart_header (chart, song)

            try
                let! response = httpclient.GetAsync("https://cdn.yavsrg.net/" + hash) |> Async.AwaitTask

                if not response.IsSuccessStatusCode then
                    return false
                else

                    use! stream = response.Content.ReadAsStreamAsync() |> Async.AwaitTask
                    use br = new BinaryReader(stream)

                    match Chart.read_headless chart.Keys header "" br with
                    | None -> return false
                    | Some chart_data ->

                    let actual_hash = Chart.hash chart_data

                    if actual_hash <> hash then
                        failwithf "Downloaded chart hash was '%s', expected '%s'" actual_hash hash

                    if File.Exists(asset_path chart.BackgroundFile cache) |> not then

                        let bg_path = Path.Combine(get_game_folder "Downloads", chart.BackgroundFile)

                        let! success =
                            download_file.RequestAsync(
                                "https://cdn.yavsrg.net/assets/" + chart.BackgroundFile,
                                bg_path,
                                ignore
                            )

                        let actual_bg_hash = hash_asset bg_path cache

                        if chart.BackgroundFile <> actual_bg_hash then
                            failwithf
                                "Downloaded background hash was '%s', expected '%s'"
                                actual_bg_hash
                                chart.BackgroundFile

                    if File.Exists(asset_path chart.AudioFile cache) |> not then

                        let audio_path = Path.Combine(get_game_folder "Downloads", chart.AudioFile)

                        let! success =
                            download_file.RequestAsync(
                                "https://cdn.yavsrg.net/assets/" + chart.AudioFile,
                                audio_path,
                                ignore
                            )

                        let actual_audio_hash = hash_asset audio_path cache

                        if chart.AudioFile <> actual_audio_hash then
                            failwithf "Downloaded audio hash was '%s', expected '%s'" actual_audio_hash chart.AudioFile

                    add_new folder [ chart_data ] cache

                    Logging.Debug(sprintf "Installed '%s' from CDN" song.FormattedTitle)

                    return true
            with err ->
                Logging.Error(err.Message, err)
                return false
        }
