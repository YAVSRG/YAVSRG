namespace Prelude.Data.Maintenance

open System.IO
open System.Collections.Generic
open Percyqaz.Common
open Prelude.Data
open Prelude.Data.Library

module Vacuum =

    let vacuum_charts (delete_missing_audio: bool, chart_db: ChartDatabase, progress: ProgressCallback) : Async<unit> =
        async {
            progress (Generic "Vacuuming")
            Logging.Debug "Running SQLite VACUUM command on charts.db"
            chart_db.VacuumSqlite()
            Logging.Debug "Chart database vacuum started"
            let unused_asset_hashes = HashSet<string>(chart_db.AssetStorage.Enumerate())
            Logging.Debug "Found %i assets on disk" unused_asset_hashes.Count

            let to_delete = ResizeArray<ChartMeta>()

            for entry in chart_db.Entries |> Seq.toArray do
                if delete_missing_audio then
                    match entry.Audio.Path with
                    | Some path when not(File.Exists(path)) -> to_delete.Add(entry)
                    | _ -> ()

                match entry.Audio with
                | AssetLocation.Hash hash -> unused_asset_hashes.Remove(hash) |> ignore
                | _ -> ()

                match entry.Background with
                | AssetLocation.Hash hash -> unused_asset_hashes.Remove(hash) |> ignore
                | _ -> ()

            if delete_missing_audio && to_delete.Count > 0 then
                progress (Generic "Deleting charts without audio")
                Logging.Debug "Found %i charts with misplaced audio files, deleting them..." to_delete.Count
                chart_db.Delete(to_delete)

            if unused_asset_hashes.Count > 0 then
                progress (Generic "Deleting unused assets")
                Logging.Debug "Found %i assets not being used by chart database, deleting them..." unused_asset_hashes.Count
                let mutable bytes_freed = 0L
                let mutable files_freed = 0

                for hash in unused_asset_hashes do
                    let freed = chart_db.AssetStorage.Remove(hash)
                    if freed >= 0 then
                        bytes_freed <- bytes_freed + freed
                        files_freed <- files_freed + 1

                Logging.Debug "Successfully deleted %i files, freeing at least %i MB" files_freed (bytes_freed / 1024L / 1024L)
            else
                Logging.Debug "No assets found to delete, you're all good!"
            progress Complete
        }