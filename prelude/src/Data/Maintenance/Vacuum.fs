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
            ChartDatabase.sqlite_vacuum chart_db
            Logging.Debug "Chart database vacuum started"
            let asset_hashes = HashSet<string>()
            for directory in Directory.EnumerateDirectories(chart_db.AssetsPath) do
                for file in Directory.EnumerateFiles(directory) do
                    let hash_file_name = Path.GetFileName(file)
                    asset_hashes.Add hash_file_name |> ignore
            Logging.Debug "Found %i assets on disk" asset_hashes.Count

            let to_delete = ResizeArray<ChartMeta>()

            for entry in chart_db.Entries |> Seq.toArray do
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
                progress (Generic "Deleting charts without audio")
                Logging.Debug "Found %i charts with misplaced audio files, deleting them..." to_delete.Count
                ChartDatabase.delete_many to_delete chart_db

            if asset_hashes.Count > 0 then
                progress (Generic "Deleting unused assets")
                Logging.Debug "Found %i assets not being used by chart database, deleting them..." asset_hashes.Count
                let mutable bytes_freed = 0L
                let mutable files_freed = 0

                for h in asset_hashes do
                    let path = ChartDatabase.asset_path h chart_db
                    try
                        let info = FileInfo(path)
                        let length = info.Length
                        info.Delete()
                        bytes_freed <- bytes_freed + length
                        files_freed <- files_freed + 1
                    with err -> Logging.Warn "Error deleting file %s: %O" path err

                Logging.Debug "Successfully deleted %i files, freeing at least %i MB" files_freed (bytes_freed / 1024L / 1024L)
            else
                Logging.Debug "No assets found to delete, you're all good!"
            progress Complete
        }