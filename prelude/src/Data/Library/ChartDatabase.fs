namespace Prelude.Data.Library

open System
open System.IO
open System.Collections.Generic
open System.Collections.Concurrent
open System.Security.Cryptography
open Percyqaz.Common
open Percyqaz.Data.Sqlite
open Prelude
open Prelude.Common
open Prelude.Charts

type ChartDatabase =
    internal
        {
            Database: Database
            Cache: ConcurrentDictionary<string, ChartMeta>
            LockObject: obj
            FastLoaded: bool
            AssetsPath: string
            mutable RecalculationNeeded: bool
        }
    member this.Entries = this.Cache.Values

module ChartDatabase =

    (* Retrival operations *)

    let get_chart (chart_id: string) (db: ChartDatabase) : Result<Chart, string> =
        DbCharts.get_chart chart_id db.Database

    let get_meta_cached (chart_id: string) (db: ChartDatabase) : ChartMeta option =
        match db.Cache.TryGetValue chart_id with
        | true, v -> Some v
        | false, _ -> None

    let get_meta (chart_id: string) (db: ChartDatabase) : ChartMeta option =
        lock db.LockObject 
        <| fun () ->
            match get_meta_cached chart_id db with
            | Some existing -> Some existing
            | None ->
                if db.FastLoaded then None
                else
                    match DbCharts.get_meta chart_id db.Database with
                    | Some chart_meta ->
                        db.Cache.[chart_id] <- chart_meta
                        Some chart_meta
                    | None -> None

    (* Insertion operations *)

    let private sha_256 = SHA256.Create()

    let private compute_hash (file_path: string) : string =
        use stream = File.OpenRead file_path
        sha_256.ComputeHash stream
        |> BitConverter.ToString
        |> fun s -> s.Replace("-", "")

    let private hash_asset (file_path: string) (db: ChartDatabase) : string =
        if not (File.Exists file_path) then
            failwithf "Missing asset file: %s" file_path

        let hash = compute_hash file_path
        let target_folder = Path.Combine(db.AssetsPath, hash.Substring(0, 2))
        Directory.CreateDirectory target_folder |> ignore
        let target_path = Path.Combine(target_folder, hash)

        if not (File.Exists target_path) then
            File.Copy(file_path, target_path)

        hash

    let asset_path (hash: string) (db: ChartDatabase) : string =
        Path.Combine(db.AssetsPath, hash.Substring(0, 2), hash)

    let import (charts: ImportChart seq) (db: ChartDatabase) =
        let moved_assets = Dictionary<string, string>()
        let now = Timestamp.now()

        lock db.LockObject <| fun () -> 
        seq {
            for chart in charts do
                let header_with_assets_moved =
                    { chart.Header with
                        BackgroundFile =
                            match chart.Header.BackgroundFile with
                            | ImportAsset.Relative s ->
                                if moved_assets.ContainsKey s then
                                    ImportAsset.Asset moved_assets.[s]
                                else
                                    let path = Path.Combine(Path.GetDirectoryName chart.LoadedFromPath, s)
                                    if not (File.Exists path) then
                                        ImportAsset.Missing
                                    else
                                        let hash = hash_asset path db
                                        moved_assets.[s] <- hash
                                        ImportAsset.Asset hash
                            | otherwise -> otherwise
                        AudioFile = 
                            match chart.Header.AudioFile with
                            | ImportAsset.Relative s ->
                                if moved_assets.ContainsKey s then
                                    ImportAsset.Asset moved_assets.[s]
                                else
                                    let path = Path.Combine(Path.GetDirectoryName chart.LoadedFromPath, s)
                                    if not (File.Exists path) then
                                        ImportAsset.Missing
                                    else
                                        let hash = hash_asset path db
                                        moved_assets.[s] <- hash
                                        ImportAsset.Asset hash
                            | otherwise -> otherwise
                    }
                let chart = { chart with Header = header_with_assets_moved }
                // todo: there should be an alternative to ChartMeta.FromImport that does the asset movement rather than the above hack
                let chart_meta = (ChartMeta.FromImport now chart)
                db.Cache.[chart_meta.Hash] <- chart_meta
                yield chart_meta, chart.Chart
        }
        |> fun charts -> DbCharts.save_batch charts db.Database

    let delete (chart_meta: ChartMeta) (db: ChartDatabase) =
        db.Cache.Remove(chart_meta.Hash) |> ignore
        DbCharts.delete chart_meta.Hash db.Database |> ignore

    let delete_many (cs: ChartMeta seq) (db: ChartDatabase) = 
        let deleted = 
            cs |> Seq.map (fun c -> db.Cache.Remove c.Hash |> ignore; c.Hash)
        DbCharts.delete_batch deleted db.Database |> ignore

    let change_packs (chart_meta: ChartMeta) (packs: Set<string>) (db: ChartDatabase) = 
        if packs.IsEmpty then
            delete chart_meta db
        else
            db.Cache.[chart_meta.Hash] <- { chart_meta with Packs = packs }
        DbCharts.update_packs_batch [chart_meta.Hash, packs] db.Database

    let delete_from_pack (chart_meta: ChartMeta) (pack: string) (db: ChartDatabase) =
        change_packs chart_meta (chart_meta.Packs.Remove pack) db

    let delete_many_from_pack (cs: ChartMeta seq) (pack: string) (db: ChartDatabase) =
        let updated =
            seq {
                for c in cs do
                    let new_packs = c.Packs.Remove pack
                    if not new_packs.IsEmpty then
                        db.Cache.[c.Hash] <- { c with Packs = new_packs }
                        yield c.Hash, new_packs
            }
        DbCharts.update_packs_batch updated db.Database
        let deleted =
            seq {
                for c in cs do
                    let new_packs = c.Packs.Remove pack
                    if new_packs.IsEmpty then
                        db.Cache.Remove(c.Hash) |> ignore
                        yield c.Hash
            }
        DbCharts.delete_batch deleted db.Database |> ignore

    (* Loading operations & Migrations *)

    let private fast_load (db: ChartDatabase) : ChartDatabase =
        lock db.LockObject
        <| fun () ->
            assert (db.Cache.Count = 0)

            for chart_meta in DbCharts.fast_load db.Database do
                db.Cache.[chart_meta.Hash] <- chart_meta

                if chart_meta.Length > 0.0f<ms> && chart_meta.Patterns.Density90 <> 0.0f</second> then
                    db.RecalculationNeeded <- false

            db

    let private migrate (db: Database) : Database =
        Database.migrate "AddChartsTable" (fun db -> DbCharts.CREATE_TABLE.Execute () db |> expect |> ignore) db
        db

    let private legacy_migrate (db: ChartDatabase) : ChartDatabase =
        
        let cache_file = Path.Combine(get_game_folder "Songs", "cache.json")
        if not (File.Exists cache_file) then db else

        Logging.Info("As part of a client update your charts are being upgraded to a new database format")
        Logging.Info("This could take a few minutes (or as long as a recache normally takes)")

        seq {
            for folder in Directory.EnumerateDirectories(get_game_folder "Songs") do
                let folder_name = Path.GetFileName(folder)
                if folder.ToLower() <> ".assets" then
                    for file in Directory.EnumerateFiles(folder) do
                        if Path.GetExtension(file).ToLower() = ".yav" then
                            match Chart.from_file folder_name file with
                            | Ok chart -> yield ChartMeta.FromImport (File.GetLastWriteTime(file) |> Timestamp.from_datetime) chart, chart.Chart
                            | Error reason -> Logging.Warn(sprintf "Failed to load %s: %s" file reason)
        }
        |> Seq.chunkBySize 1000
        |> Seq.iter (fun chunk ->
            DbCharts.save_batch chunk db.Database
        )

        Logging.Info("Written charts to database in new format")
        File.Move(cache_file, cache_file + ".old")
        Logging.Info("Marked cache.json as old. All done!")
        db

    // Fast load true loads the contents of the db into memory (used by game client)
    // Fast load false may be used for tools that just want to fetch stuff for a couple of charts
    let create (use_fast_load: bool) (database: Database) : ChartDatabase =
        legacy_migrate
            {
                Database = migrate database
                Cache = ConcurrentDictionary()
                LockObject = obj ()
                FastLoaded = use_fast_load
                AssetsPath = Path.Combine(get_game_folder "Songs", ".assets")
                RecalculationNeeded = use_fast_load
            }
        |> if use_fast_load then fast_load else id

    (* Download from CDN *)

    open Prelude.Backbeat.Archive
    open Prelude.Data.WebServices
    open System.Net.Http

    let private httpclient = new HttpClient()

    let cdn_download (folder: string) (hash: string) (chart: Chart, song: Song) (db: ChartDatabase) =
        async {
            let header = Archive.make_chart_header (chart, song)

            try
                let! response = httpclient.GetAsync("https://cdn.yavsrg.net/" + hash) |> Async.AwaitTask

                if not response.IsSuccessStatusCode then
                    return false
                else

                    use! stream = response.Content.ReadAsStreamAsync() |> Async.AwaitTask
                    use br = new BinaryReader(stream)

                    match Chart.read_headless chart.Keys br with
                    | Error reason ->
                        Logging.Error(sprintf "CDN download invalid for %s: %s" hash reason)
                        return false
                    | Ok chart_data ->

                    let actual_hash = Chart.hash chart_data

                    if actual_hash <> hash then
                        failwithf "Downloaded chart hash was '%s', expected '%s'" actual_hash hash

                    if File.Exists(asset_path chart.BackgroundHash db) |> not then

                        let bg_path = Path.Combine(get_game_folder "Downloads", chart.BackgroundHash)

                        let! success =
                            download_file.RequestAsync(
                                "https://cdn.yavsrg.net/assets/" + chart.BackgroundHash,
                                bg_path,
                                ignore
                            )

                        let actual_bg_hash = hash_asset bg_path db

                        if chart.BackgroundHash <> actual_bg_hash then
                            failwithf
                                "Downloaded background hash was '%s', expected '%s'"
                                actual_bg_hash
                                chart.BackgroundHash

                    if File.Exists(asset_path chart.AudioHash db) |> not then

                        let audio_path = Path.Combine(get_game_folder "Downloads", chart.AudioHash)

                        let! success =
                            download_file.RequestAsync(
                                "https://cdn.yavsrg.net/assets/" + chart.AudioHash,
                                audio_path,
                                ignore
                            )

                        let actual_audio_hash = hash_asset audio_path db

                        if chart.AudioHash <> actual_audio_hash then
                            failwithf "Downloaded audio hash was '%s', expected '%s'" actual_audio_hash chart.AudioHash

                    import [{ PackName = folder; LoadedFromPath = ""; Header = header; Chart = chart_data }] db

                    Logging.Debug(sprintf "Installed '%s' from CDN" song.FormattedTitle)

                    return true
            with err ->
                Logging.Error(err.Message, err)
                return false
        }

    let vacuum =
        { new Async.Service<ChartDatabase * bool, unit>() with
            override this.Handle((charts_db, delete_missing_audio)) =
                async {
                    Logging.Debug("Chart database vacuum started")
                    let asset_hashes = HashSet<string>()
                    for directory in Directory.EnumerateDirectories(charts_db.AssetsPath) do
                        for file in Directory.EnumerateFiles(directory) do
                            let hash_file_name = Path.GetFileName(file)
                            asset_hashes.Add hash_file_name |> ignore
                    Logging.Debug(sprintf "Found %i assets on disk" asset_hashes.Count)

                    let to_delete = ResizeArray<ChartMeta>()

                    for entry in charts_db.Entries do
                        if delete_missing_audio then
                            match entry.Audio.Path with
                            | Some p when not (File.Exists p) -> to_delete.Add(entry)
                            | _ -> ()

                        match entry.Audio with
                        | AssetPath.Hash h -> asset_hashes.Remove h |> ignore
                        | _ -> ()

                        match entry.Background with
                        | AssetPath.Hash h -> asset_hashes.Remove h |> ignore
                        | _ -> ()

                    if delete_missing_audio && to_delete.Count > 0 then
                        Logging.Debug(sprintf "Found %i charts with misplaced audio files, deleting them..." to_delete.Count)
                        delete_many to_delete charts_db

                    if asset_hashes.Count > 0 then
                        Logging.Debug(sprintf "Found %i assets not being used by chart database, deleting them..." asset_hashes.Count)
                        let mutable bytes_freed = 0L
                        let mutable files_freed = 0

                        for h in asset_hashes do
                            let path = asset_path h charts_db
                            try
                                let info = FileInfo(path)
                                let length = info.Length
                                info.Delete()
                                bytes_freed <- bytes_freed + length
                                files_freed <- files_freed + 1
                            with err -> Logging.Warn(sprintf "Error deleting file %s" path, err)

                        Logging.Debug(sprintf "Successfully deleted %i files, freeing at least %i MB" files_freed (bytes_freed / 1024L / 1024L))
                    else
                        Logging.Debug(sprintf "No assets found to delete, you're all good!")
                }
        }
    
    open Prelude.Charts.Processing.Difficulty
    open Prelude.Charts.Processing.Patterns

    let recalculate_data =
        { new Async.Service<ChartDatabase, unit>() with
            override this.Handle(charts_db) =
                async {
                    seq {
                        for entry in charts_db.Entries do
                            match get_chart entry.Hash charts_db with
                            | Ok chart ->
                                yield entry.Hash, float32 (DifficultyRating.calculate 1.0f<rate> chart.Notes).Physical, PatternReport.from_chart chart
                            | Error reason -> Logging.Warn(sprintf "Error recalculating patterns for %s: %s" entry.Hash reason)
                    }
                    |> Seq.chunkBySize 1000
                    |> Seq.iter (fun chunk ->
                        DbCharts.update_calculated_data chunk charts_db.Database
                    )
                }
        }

    let recalculate_if_needed (db: ChartDatabase) (recache_complete_callback: unit -> unit) : bool =
        if db.RecalculationNeeded then
            db.RecalculationNeeded <- false
            recalculate_data.Request(db, recache_complete_callback)
            true
        else false