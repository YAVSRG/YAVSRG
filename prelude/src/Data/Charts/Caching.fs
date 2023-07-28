namespace Prelude.Data.Charts.Caching

open System
open System.IO
open System.Collections.Generic
open System.Collections.Concurrent
open System.Security.Cryptography
open Percyqaz.Common
open Percyqaz.Json
open Prelude
open Prelude.Charts.Formats.Interlude
open Prelude.Charts.Formats.Conversions
open Prelude.Gameplay.Layout
open Prelude.Gameplay.Difficulty

[<Json.AutoCodec>]
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
        BPM: (float32<ms/beat> * float32<ms/beat>)
        DifficultyName: string
        Physical: float
        BackgroundFile: string option
        AudioFile: string option
    }
    member this.Key = sprintf "%s/%s" this.Folder this.Hash

type Cache = 
    {   
        RootPath: string
        Entries: ConcurrentDictionary<string, CachedChart>
    }

module Cache =

    (* IO operations *)

    let from_path (path: string) =
        Directory.CreateDirectory path |> ignore
        File.WriteAllText(Path.Combine(path, "HOW_TO_ADD_SONGS.txt"),
            "Dragging and dropping things into this folder won't work.\n" +
            "Instead, drag and drop things onto the *game window* while it's open and it will import.\n\n" +
            "Does this folder have stuff in, but they don't show up in game?\n" +
            "In that case, check they are .yav files, and then go to Options > Debug > Rebuild cache and let that run.")

        {
            RootPath = path
            Entries = loadImportantJsonFile "Cache" (Path.Combine(path, "cache.json")) false
        }

    let save (cache: Cache) =
        saveImportantJsonFile (Path.Combine(cache.RootPath, "cache.json")) cache.Entries

    let create_entry (folder_name: string) (file_time: DateTime) (chart: Chart) =
        let lastNote = chart.LastNote
        let rating = RatingReport(chart.Notes, 1.0f, Layout.Spread, chart.Keys)
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
            Length = lastNote - chart.FirstNote
            DateAdded = file_time
            BPM = Interlude.minMaxBPM (List.ofSeq chart.BPM) lastNote
            DifficultyName = chart.Header.DiffName
            Physical = rating.Physical
            BackgroundFile = match chart.Header.BackgroundFile with Asset s -> Some s | _ -> None
            AudioFile = match chart.Header.AudioFile with Asset s -> Some s | _ -> None
        }

    let private sha_256 = SHA256.Create()

    let hash_asset (file_path: string) (cache: Cache) : string =
        if not (File.Exists file_path) then failwithf "Missing asset file: %s" file_path
        let stream = File.OpenRead(file_path)
        let hash = sha_256.ComputeHash stream |> BitConverter.ToString |> fun s -> s.Replace("-", "")
        stream.Dispose()
        let target_folder = Path.Combine(cache.RootPath, ".assets", hash.Substring(0, 2))
        Directory.CreateDirectory target_folder |> ignore
        let target_path = Path.Combine(target_folder, hash)
        if File.Exists(target_path) then 
            Logging.Info(sprintf "Asset %s is already stored" hash)
            File.Delete(file_path)
        else File.Move(file_path, target_path)
        hash

    (* Using the cache *)

    let get_path (entry: CachedChart) (cache: Cache) = Path.Combine(cache.RootPath, entry.Folder, entry.Hash + ".yav")
    
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

    let recache_service =
        { new Async.Service<Cache, unit>() with
            override this.Handle(cache: Cache) =
                async {
                    Logging.Debug(sprintf "Rebuilding chart cache @ %s" cache.RootPath)
                    cache.Entries.Clear()
                    for folder in Directory.EnumerateDirectories cache.RootPath |> Seq.filter (fun p -> Path.GetFileName p <> ".assets") do

                        // backwards compatible stuff. move all loose folders
                        for song in Directory.EnumerateDirectories folder do
                            let moved_assets = Dictionary<string, string>() // relative file name -> hash
                            for file in Directory.EnumerateFiles song do
                                match Path.GetExtension(file).ToLower() with
                                | ".yav" ->
                                    match Chart.fromFile file with
                                    | Some c ->
                                        try
                                            let c = 
                                                { c with
                                                    Header = 
                                                        { c.Header with
                                                            BackgroundFile = 
                                                                match c.Header.BackgroundFile with
                                                                | Relative s ->
                                                                    if moved_assets.ContainsKey s then Asset moved_assets.[s]
                                                                    else
                                                                        let path = (background_path c cache).Value
                                                                        if not (File.Exists path) then Missing else
                                                                        let hash = hash_asset path cache
                                                                        moved_assets.[s] <- hash
                                                                        Asset hash
                                                                | Absolute s -> Absolute s
                                                                | Asset s -> Asset s
                                                                | Missing -> Missing
                                                            
                                                            AudioFile = 
                                                                match c.Header.AudioFile with
                                                                | Relative s ->
                                                                    if moved_assets.ContainsKey s then Asset moved_assets.[s]
                                                                    else 
                                                                        let path = (audio_path c cache).Value
                                                                        if not (File.Exists path) then Missing else
                                                                        let hash = hash_asset path cache
                                                                        moved_assets.[s] <- hash
                                                                        Asset hash
                                                                | Absolute s -> Absolute s
                                                                | Asset s -> Asset s
                                                                | Missing -> Missing
                                                        }
                                                }
                                            let entry = create_entry (Path.GetFileName folder) (File.GetCreationTimeUtc file) c
                                            let new_file = get_path entry cache
                                            Chart.toFile c new_file
                                            File.SetCreationTimeUtc(new_file, entry.DateAdded)
                                            File.Delete file

                                            cache.Entries.[entry.Key] <- entry
                                        with err -> Logging.Error(sprintf "Error (likely assets) with caching a legacy yav file: %s" file, err)
                                    | None -> ()
                                | _ -> ()
                            try Directory.Delete song with err -> Logging.Warn(sprintf "Error deleting should-be-empty folder: %s" song, err)

                        // current system. just very minor sanity checks then straight into cache
                        for file in Directory.EnumerateFiles folder do
                            match Path.GetExtension(file).ToLower() with
                            | ".yav" ->
                                match Chart.fromFile file with
                                | Some c -> 
                                    let entry = create_entry (Path.GetFileName folder) (File.GetCreationTimeUtc file) c
                                    match c.Header.BackgroundFile with
                                    | Relative _ -> Logging.Warn(sprintf ".yav files in the cache (%s) shouldn't have relative assets, this is a legacy feature" file)
                                    | _ -> ()
                                        
                                    match c.Header.AudioFile with
                                    | Relative _ -> Logging.Warn(sprintf ".yav files in the cache (%s) shouldn't have relative assets, this is a legacy feature" file)
                                    | _ -> ()

                                    let intended_path = get_path entry cache
                                    if intended_path <> file then Logging.Debug(sprintf "Moving cache file from %s to %s" file intended_path); File.Move(file, intended_path)

                                    cache.Entries.[entry.Key] <- entry
                                | None -> ()
                            | _ -> ()
                    save cache
                }
        }

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
                                    if moved_assets.ContainsKey s then Asset moved_assets.[s]
                                    else 
                                        let path = (background_path c cache).Value
                                        if not (File.Exists path) then Missing else
                                        let hash = hash_asset path cache
                                        moved_assets.[s] <- hash
                                        Asset hash
                                | Absolute s -> Absolute s
                                | Asset s -> Asset s
                                | Missing -> Missing
                                
                            AudioFile = 
                                match c.Header.AudioFile with
                                | Relative s ->
                                    if moved_assets.ContainsKey s then Asset moved_assets.[s]
                                    else 
                                        let path = (audio_path c cache).Value
                                        if not (File.Exists path) then Missing else
                                        let hash = hash_asset path cache
                                        moved_assets.[s] <- hash
                                        Asset hash
                                | Absolute s -> Absolute s
                                | Asset s -> Asset s
                                | Missing -> Missing
                        }
                }
            let entry = create_entry folder now c
            let new_file = get_path entry cache
            Chart.toFile c new_file

            cache.Entries.[entry.Key] <- entry
        save cache
    
    let load (entry: CachedChart) (cache: Cache) =
        get_path entry cache |> Chart.fromFile

    let by_key (key: string) (cache: Cache) : CachedChart option =
        let success, c = cache.Entries.TryGetValue key
        if success then Some c else None

    let by_hash(id: string) (cache: Cache) : CachedChart option =
        Seq.tryFind (fun cc -> cc.Hash = id) cache.Entries.Values

    let delete (c: CachedChart) (cache: Cache) =
        cache.Entries.TryRemove c.Key |> ignore
        let path = get_path c cache
        if File.Exists path then File.Delete path
        // todo: remove assets IF they aren't used by anything else in the cache
    
    let deleteMany (cs: CachedChart seq) (cache: Cache) = Seq.iter (fun c -> delete c cache) cs