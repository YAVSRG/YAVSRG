namespace Prelude.Data.Library

open System
open System.IO
open System.Collections.Generic
open System.Collections.Concurrent
open System.Security.Cryptography
open Percyqaz.Common
open Percyqaz.Data.Sqlite
open Prelude
open Prelude.Charts

module private ChartDatabaseMigrations =
    let migrate (db: Database) : Database =
        Database.migrate "AddChartsTable" (fun db -> DbCharts.CREATE_TABLE.Execute () db |> expect |> ignore) db
        Database.migrate "ResetOriginsColumn" (fun db -> DbCharts.RESET_ORIGINS.Execute () db |> expect |> ignore) db
        Database.migrate "ResetOriginsColumn2" (fun db -> DbCharts.RESET_ORIGINS.Execute () db |> expect |> ignore) db

        Database.migrate
            "ResetOriginsColumn3"
            (fun db -> DbCharts.RESET_OSU_QUAVER_ORIGINS.Execute () db |> expect |> ignore)
            db

        db

[<AbstractClass>]
type AssetStorage() =
    abstract member Add: string -> string
    abstract member Remove: string -> int64
    abstract member Contains: string -> bool
    abstract member GetPath: string -> string
    abstract member Enumerate: unit -> string seq

type FileSystemAssetStorage(assets_path: string) =
    inherit AssetStorage()

    override this.Add(file_path: string) : string =

        let inline compute_asset_hash (file_path: string) : string =
            use stream = File.OpenRead(file_path)
            SHA256.HashData(stream) |> BitConverter.ToString |> _.Replace("-", "")

        let hash = compute_asset_hash(file_path)

        let target_folder = Path.Combine(assets_path, hash.Substring(0, 2))
        let target_path = Path.Combine(target_folder, hash)

        Directory.CreateDirectory(target_folder) |> ignore

        if not(File.Exists(target_path)) then
            File.Copy(file_path, target_path)

        hash

    override this.Remove(hash: string) : int64 =
        let path = this.GetPath(hash)

        try
            let info = FileInfo(path)
            let length = info.Length
            info.Delete()
            length
        with err ->
            Logging.Warn "Error deleting file %s: %O" path err
            -1L

    override this.Contains(hash: string) : bool = File.Exists(this.GetPath(hash))

    override this.GetPath(hash: string) : string =
        Path.Combine(assets_path, hash.Substring(0, 2), hash)

    override this.Enumerate() : string seq =
        seq {
            for directory in Directory.EnumerateDirectories(assets_path) do
                for file in Directory.EnumerateFiles(directory) do
                    let hash_file_name = Path.GetFileName(file)
                    yield hash_file_name
        }

type ChartDatabase =
    internal
        {
            Database: Database
            Cache: ConcurrentDictionary<string, ChartMeta>
            LockObject: obj
            FastLoaded: bool
            AssetStorage: AssetStorage
            mutable RecalculationNeeded: bool
        }
    member this.Entries = this.Cache.Values

    member this.GetChart(chart_id: string) : Result<Chart, string> =
        DbCharts.get_chart chart_id this.Database

    member this.TryGetCachedChartMeta(chart_id: string) : ChartMeta option =
        match this.Cache.TryGetValue chart_id with
        | true, v -> Some v
        | false, _ -> None

    member this.GetChartMeta(chart_id: string) : ChartMeta option =
        lock this.LockObject
        <| fun () ->

            let inline fetch_chart_meta () : ChartMeta option =
                match DbCharts.get_meta chart_id this.Database with
                | Some chart_meta ->
                    this.Cache.[chart_id] <- chart_meta
                    Some chart_meta
                | None -> None

            match this.TryGetCachedChartMeta(chart_id) with
            | Some existing -> Some existing
            | None -> if this.FastLoaded then None else fetch_chart_meta()

    member this.Import(imported_charts: ImportChart seq) : unit =

        let already_moved = Dictionary<string, string>()

        let inline copy_asset (absolute_path: string) : AssetLocation =
            if already_moved.ContainsKey(absolute_path) then
                AssetLocation.Hash already_moved.[absolute_path]

            elif File.Exists(absolute_path) then
                let hash = this.AssetStorage.Add(absolute_path)
                already_moved.[absolute_path] <- hash
                AssetLocation.Hash hash

            else
                AssetLocation.Missing

        let inline import_asset (asset: ImportAsset) : AssetLocation =
            match asset with
            | ImportAsset.Copy absolute_path -> copy_asset(absolute_path)

            | ImportAsset.Link absolute_path ->
                if File.Exists(absolute_path) then AssetLocation.Absolute(absolute_path) else AssetLocation.Missing

            | ImportAsset.Download hash -> AssetLocation.Hash hash
            | ImportAsset.Missing -> AssetLocation.Missing

        let inline merge_incoming_chart_with_existing
            (incoming_chart: Chart, incoming_meta: ChartMeta)
            : Chart * ChartMeta =
            match this.Cache.TryGetValue(incoming_meta.Hash) with
            | false, _ -> incoming_chart, incoming_meta
            | true, existing_meta ->

                let merged_meta, existing_takes_priority =
                    incoming_meta.MergeWithExisting(existing_meta)

                if existing_takes_priority then
                    match this.GetChart(incoming_meta.Hash) with
                    | Error _ -> incoming_chart, merged_meta
                    | Ok existing_chart -> existing_chart, merged_meta

                else
                    incoming_chart, merged_meta

        let now = Timestamp.now()

        lock this.LockObject
        <| fun () ->
            seq {
                for import_chart in imported_charts do
                    let incoming_meta = ChartMeta.CreateFromImport now import_asset import_chart
                    let incoming_chart = import_chart.Chart

                    let accepted_chart, accepted_meta =
                        merge_incoming_chart_with_existing(incoming_chart, incoming_meta)

                    this.Cache.[accepted_meta.Hash] <- accepted_meta
                    yield accepted_meta, accepted_chart
            }
            |> fun charts -> DbCharts.save_batch charts this.Database

    member this.Delete(chart_meta: ChartMeta) : unit =
        lock this.LockObject
        <| fun () ->

            this.Cache.Remove(chart_meta.Hash) |> ignore
            DbCharts.delete chart_meta.Hash this.Database |> ignore

    member this.Delete(chart_metas: ChartMeta seq) : unit =
        lock this.LockObject
        <| fun () ->

            for chart_meta in chart_metas do
                this.Cache.Remove chart_meta.Hash |> ignore

            DbCharts.delete_batch (Seq.map _.Hash chart_metas) this.Database |> ignore

    member private this.ChangePacks(chart_meta: ChartMeta, packs: Set<string>) : unit =
        lock this.LockObject
        <| fun () ->

            if packs.IsEmpty then
                this.Delete(chart_meta)
            else
                this.Cache.[chart_meta.Hash] <- { chart_meta with Packs = packs }
                DbCharts.update_packs_batch [ chart_meta.Hash, packs ] this.Database

    member this.AddToPack(chart_meta: ChartMeta, pack: string) : unit =
        this.ChangePacks(chart_meta, chart_meta.Packs.Add(pack))

    member this.RemoveFromPack(chart_meta: ChartMeta, pack: string) : unit =
        this.ChangePacks(chart_meta, chart_meta.Packs.Remove(pack))

    member this.RemoveFromPack(chart_metas: ChartMeta seq, pack: string) : unit =
        lock this.LockObject
        <| fun () ->

            let still_in_packs = ResizeArray<string * Set<string>>()
            let now_in_no_packs = ResizeArray<string>()

            for chart_meta in chart_metas do
                let remaining_packs = chart_meta.Packs.Remove(pack)

                if remaining_packs.IsEmpty then
                    this.Cache.Remove(chart_meta.Hash) |> ignore
                    now_in_no_packs.Add(chart_meta.Hash)

                else
                    this.Cache.[chart_meta.Hash] <- { chart_meta with Packs = remaining_packs }
                    still_in_packs.Add(chart_meta.Hash, remaining_packs)

            DbCharts.update_packs_batch still_in_packs this.Database
            DbCharts.delete_batch now_in_no_packs this.Database |> ignore

    member inline internal this.VacuumSqlite() : unit =
        lock this.LockObject <| fun () -> Database.exec_raw "VACUUM;" this.Database |> ignore

    static member CreateFullyLoaded(database: Database, asset_storage: AssetStorage) : ChartDatabase =

        let inline chart_has_pattern_data (chart_meta: ChartMeta) =
            chart_meta.Length > 0.0f<ms> && chart_meta.Patterns.Density90 <> 0.0f< / rate>

        let inline load_charts_into_cache (cache: ConcurrentDictionary<string, ChartMeta>) : bool =
            let mutable recalculation_needed = true

            for chart_meta in DbCharts.fast_load database do
                if recalculation_needed && chart_has_pattern_data(chart_meta) then
                    recalculation_needed <- false

                cache.[chart_meta.Hash] <- chart_meta

            if cache.Count = 0 then
                recalculation_needed <- false

            recalculation_needed

        let migrated_database = ChartDatabaseMigrations.migrate(database)
        let cache = ConcurrentDictionary()
        let recalculation_needed = load_charts_into_cache(cache)

        {
            Database = migrated_database
            Cache = cache
            LockObject = obj()
            FastLoaded = true
            AssetStorage = asset_storage
            RecalculationNeeded = recalculation_needed
        }

    static member CreateFullyLoaded(database: Database) : ChartDatabase =
        let default_asset_storage =
            FileSystemAssetStorage(Path.Combine(get_game_folder "Songs", ".assets"))

        ChartDatabase.CreateFullyLoaded(database, default_asset_storage)

    static member CreateLazyLoaded(database: Database, asset_storage: AssetStorage) : ChartDatabase =
        let migrated_database = ChartDatabaseMigrations.migrate(database)

        {
            Database = migrated_database
            Cache = ConcurrentDictionary()
            LockObject = obj()
            FastLoaded = false
            AssetStorage = asset_storage
            RecalculationNeeded = false
        }

    static member CreateLazyLoaded(database: Database) : ChartDatabase =
        let default_asset_storage =
            FileSystemAssetStorage(Path.Combine(get_game_folder "Songs", ".assets"))

        ChartDatabase.CreateLazyLoaded(database, default_asset_storage)
