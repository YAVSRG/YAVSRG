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

    member this.GetChart(chart_id: string) : Result<Chart, string> =
        DbCharts.get_chart chart_id this.Database

    member this.TryGetCachedChartMeta(chart_id: string) : ChartMeta option =
        match this.Cache.TryGetValue chart_id with
        | true, v -> Some v
        | false, _ -> None

    member this.GetChartMeta(chart_id: string) : ChartMeta option =
        lock this.LockObject <| fun () ->
            
        let inline fetch_chart_meta() : ChartMeta option =
            match DbCharts.get_meta chart_id this.Database with
            | Some chart_meta ->
                this.Cache.[chart_id] <- chart_meta
                Some chart_meta
            | None -> None
            
        match this.TryGetCachedChartMeta(chart_id) with
        | Some existing -> Some existing
        | None -> if this.FastLoaded then None else fetch_chart_meta()

module ChartDatabase =

    (* Insertion operations *)

    let asset_path (hash: string) (db: ChartDatabase) : string =
        Path.Combine(db.AssetsPath, hash.Substring(0, 2), hash)

    let inline internal hash_and_store_asset (file_path: string) (db: ChartDatabase) : string =
        
        let inline compute_asset_hash (file_path: string) : string =
            use stream = File.OpenRead(file_path)
            SHA256.HashData(stream)
            |> BitConverter.ToString
            |> _.Replace("-", "")
            
        let hash = compute_asset_hash(file_path)
        
        let target_folder = Path.Combine(db.AssetsPath, hash.Substring(0, 2))
        let target_path = Path.Combine(target_folder, hash)
        
        Directory.CreateDirectory(target_folder) |> ignore
        if not (File.Exists(target_path)) then
            File.Copy(file_path, target_path)

        hash

    let import (imported_charts: ImportChart seq) (db: ChartDatabase) : unit =

        let already_moved = Dictionary<string, string>()
        let inline copy_asset (absolute_path: string) : AssetPath =
            if already_moved.ContainsKey(absolute_path) then
                AssetPath.Hash already_moved.[absolute_path]
                
            elif File.Exists(absolute_path) then
                let hash = hash_and_store_asset(absolute_path) db
                already_moved.[absolute_path] <- hash
                AssetPath.Hash hash
                
            else
                AssetPath.Missing
        
        let inline import_asset (asset: ImportAsset) : AssetPath =
            match asset with
            | ImportAsset.Copy absolute_path -> copy_asset(absolute_path)
            
            | ImportAsset.Link absolute_path ->
                if File.Exists(absolute_path) then AssetPath.Absolute(absolute_path)
                else AssetPath.Missing

            | ImportAsset.Download hash -> AssetPath.Hash hash
            | ImportAsset.Missing -> AssetPath.Missing
            
        let inline merge_incoming_chart_with_existing (incoming_chart: Chart, incoming_meta: ChartMeta) : Chart * ChartMeta =
            match db.Cache.TryGetValue(incoming_meta.Hash) with
            | false, _ -> incoming_chart, incoming_meta
            | true, existing_meta ->
                
                let merged_meta, existing_takes_priority =
                    incoming_meta.MergeWithExisting(existing_meta)

                if existing_takes_priority then
                    match db.GetChart(incoming_meta.Hash) with
                    | Error _ -> incoming_chart, merged_meta
                    | Ok existing_chart -> existing_chart, merged_meta
                    
                else incoming_chart, merged_meta

        let now = Timestamp.now()

        lock db.LockObject <| fun () ->
        seq {
            for import_chart in imported_charts do
                let incoming_meta = ChartMeta.CreateFromImport now import_asset import_chart
                let incoming_chart = import_chart.Chart

                let accepted_chart, accepted_meta =
                    merge_incoming_chart_with_existing(incoming_chart, incoming_meta)

                db.Cache.[accepted_meta.Hash] <- accepted_meta
                yield accepted_meta, accepted_chart
        }
        |> fun charts -> DbCharts.save_batch charts db.Database

    let delete (chart_meta: ChartMeta) (db: ChartDatabase) : unit =
        lock db.LockObject <| fun () ->
            
        db.Cache.Remove(chart_meta.Hash) |> ignore
        DbCharts.delete chart_meta.Hash db.Database |> ignore

    let delete_many (chart_metas: ChartMeta seq) (db: ChartDatabase) : unit =
        lock db.LockObject <| fun () ->
            
        for chart_meta in chart_metas do
            db.Cache.Remove chart_meta.Hash |> ignore
            
        DbCharts.delete_batch (Seq.map _.Hash chart_metas) db.Database |> ignore

    let inline internal change_packs (chart_meta: ChartMeta) (packs: Set<string>) (db: ChartDatabase) : unit =
        lock db.LockObject <| fun () ->
            
        if packs.IsEmpty then delete chart_meta db
        else db.Cache.[chart_meta.Hash] <- { chart_meta with Packs = packs }
        
        DbCharts.update_packs_batch [chart_meta.Hash, packs] db.Database
        
    let add_to_pack (chart_meta: ChartMeta) (pack: string) (db: ChartDatabase) : unit =
        change_packs chart_meta (chart_meta.Packs.Add pack) db

    let remove_from_pack (chart_meta: ChartMeta) (pack: string) (db: ChartDatabase) : unit =
        change_packs chart_meta (chart_meta.Packs.Remove pack) db

    let remove_many_from_pack (chart_metas: ChartMeta seq) (pack: string) (db: ChartDatabase) : unit =
        lock db.LockObject <| fun () ->
            
        let still_in_packs = ResizeArray<string * Set<string>>()
        let now_in_no_packs = ResizeArray<string>()
        
        for chart_meta in chart_metas do
            let remaining_packs = chart_meta.Packs.Remove(pack)
            if remaining_packs.IsEmpty then
                now_in_no_packs.Add(chart_meta.Hash)
            else
                still_in_packs.Add(chart_meta.Hash, remaining_packs)
                
        DbCharts.update_packs_batch still_in_packs db.Database
        DbCharts.delete_batch now_in_no_packs db.Database |> ignore

    (* Loading operations & Migrations *)

    let sqlite_vacuum (db: ChartDatabase) : unit =
        lock db.LockObject <| fun () ->
        Database.exec_raw "VACUUM;" db.Database |> ignore

    let private fast_load (db: ChartDatabase) : ChartDatabase =
        lock db.LockObject
        <| fun () ->
            assert (db.Cache.Count = 0)

            for chart_meta in DbCharts.fast_load db.Database do
                db.Cache.[chart_meta.Hash] <- chart_meta

                if chart_meta.Length > 0.0f<ms> && chart_meta.Patterns.Density90 <> 0.0f</rate> then
                    db.RecalculationNeeded <- false

            if db.Cache.Count = 0 then db.RecalculationNeeded <- false

            db

    let private migrate (db: Database) : Database =
        Database.migrate "AddChartsTable" (fun db -> DbCharts.CREATE_TABLE.Execute () db |> expect |> ignore) db
        Database.migrate "ResetOriginsColumn" (fun db -> DbCharts.RESET_ORIGINS.Execute () db |> expect |> ignore) db
        Database.migrate "ResetOriginsColumn2" (fun db -> DbCharts.RESET_ORIGINS.Execute () db |> expect |> ignore) db
        Database.migrate "ResetOriginsColumn3" (fun db -> DbCharts.RESET_OSU_QUAVER_ORIGINS.Execute () db |> expect |> ignore) db
        db

    // Fast load true loads the contents of the db into memory (used by game client)
    // Fast load false may be used for tools that just want to fetch stuff for a couple of charts
    let create (use_fast_load: bool) (database: Database) : ChartDatabase =
        {
            Database = migrate database
            Cache = ConcurrentDictionary()
            LockObject = obj ()
            FastLoaded = use_fast_load
            AssetsPath = Path.Combine(get_game_folder "Songs", ".assets")
            RecalculationNeeded = use_fast_load
        }
        |> if use_fast_load then fast_load else id