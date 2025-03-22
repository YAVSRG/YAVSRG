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

module ChartDatabase =

    (* Retrieval operations *)

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

    let internal hash_asset (file_path: string) (db: ChartDatabase) : string =
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

    let import_asset (already_moved: Dictionary<string, string>) (asset: ImportAsset) (db: ChartDatabase) : AssetPath =
        match asset with

        // Copy asset from source into the .assets folder
        | ImportAsset.Copy absolute_path ->
            if already_moved.ContainsKey absolute_path then
                AssetPath.Hash already_moved.[absolute_path]
            elif File.Exists absolute_path then
                let hash = hash_asset absolute_path db
                already_moved.[absolute_path] <- hash
                AssetPath.Hash hash
            else
                AssetPath.Missing

        // Use asset in-place from source folder
        | ImportAsset.Link absolute_path ->
            if File.Exists absolute_path then
                AssetPath.Absolute absolute_path
            else
                AssetPath.Missing

        // No action needed, asset has already been downloaded into .assets
        | ImportAsset.Download hash -> AssetPath.Hash hash

        | ImportAsset.Missing -> AssetPath.Missing

    let import (imported_charts: ImportChart seq) (db: ChartDatabase) =
        let already_moved = Dictionary<string, string>()
        let now = Timestamp.now()

        lock db.LockObject <| fun () ->
        seq {
            for import_chart in imported_charts do
                let incoming_meta = ChartMeta.FromImport now (fun x -> import_asset already_moved x db) import_chart
                let incoming_chart = import_chart.Chart

                let accepted_chart, accepted_chart_meta =
                    match db.Cache.TryGetValue incoming_meta.Hash with
                    | false, _ -> incoming_chart, incoming_meta
                    | true, data ->
                        let merged_chart_meta, accept_updated_chart = incoming_meta.MergeWithExisting data

                        if accept_updated_chart then incoming_chart, merged_chart_meta
                        else
                            match get_chart incoming_meta.Hash db with
                            | Error _ -> incoming_chart, merged_chart_meta
                            | Ok existing_chart -> existing_chart, merged_chart_meta

                db.Cache.[incoming_meta.Hash] <- accepted_chart_meta
                yield accepted_chart_meta, accepted_chart
        }
        |> fun charts -> DbCharts.save_batch charts db.Database

    let delete (chart_meta: ChartMeta) (db: ChartDatabase) =
        lock db.LockObject <| fun () ->
        db.Cache.Remove(chart_meta.Hash) |> ignore
        DbCharts.delete chart_meta.Hash db.Database |> ignore

    let delete_many (cs: ChartMeta seq) (db: ChartDatabase) =
        lock db.LockObject <| fun () ->
        let deleted =
            cs |> Seq.map (fun c -> db.Cache.Remove c.Hash |> ignore; c.Hash)
        DbCharts.delete_batch deleted db.Database |> ignore

    let change_packs (chart_meta: ChartMeta) (packs: Set<string>) (db: ChartDatabase) =
        lock db.LockObject <| fun () ->
        if packs.IsEmpty then
            delete chart_meta db
        else
            db.Cache.[chart_meta.Hash] <- { chart_meta with Packs = packs }
        DbCharts.update_packs_batch [chart_meta.Hash, packs] db.Database

    let delete_from_pack (chart_meta: ChartMeta) (pack: string) (db: ChartDatabase) =
        change_packs chart_meta (chart_meta.Packs.Remove pack) db

    let delete_many_from_pack (cs: ChartMeta seq) (pack: string) (db: ChartDatabase) =
        lock db.LockObject <| fun () ->
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

    let sqlite_vacuum (db: ChartDatabase) =
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

    let private legacy_migrate (db: ChartDatabase) : ChartDatabase =
        // This used to move charts from the old folder structure to the new sqlite database system
        // It is no longer supported; latest version that supported it was 0.7.27.10
        // If updating an Interlude instance from before then you should instead just import charts fresh (and delete the leftover files)
        let old_cache_file = Path.Combine(get_game_folder "Songs", "cache.json.old")
        File.Delete old_cache_file
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