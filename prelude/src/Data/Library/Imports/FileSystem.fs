namespace Prelude.Data.Library.Imports

open System.IO
open System.IO.Compression
open Percyqaz.Common
open Prelude.Charts
open Prelude.Formats
open Prelude.Data
open Prelude.Data.Library
open Prelude.Data.User

module Imports =

    let delete_folder =
        { new Async.Queue<string, bool>() with
            override this.Handle(path) =
                async {
                    Logging.Debug "Deleting folder '%s' post-import" path
                    let mutable remaining_retries = 3
                    while remaining_retries > 0 do
                        try
                            Directory.Delete (path, true)
                            remaining_retries <- -1
                        with err ->
                            remaining_retries <- remaining_retries - 1
                            Logging.Debug "Deleting folder '%s' failed, retrying in 1s: %s" path err.Message
                            System.Threading.Thread.Sleep(1000)
                    if remaining_retries = 0 then Logging.Error "Deleting folder '%s' failed, out of retries" path
                    return remaining_retries < 0
                }
        }

    let delete_file =
        { new Async.Queue<string, bool>() with
            override this.Handle(path) =
                async {
                    Logging.Debug "Deleting file '%s' post-import" path
                    let mutable remaining_retries = 3
                    while remaining_retries > 0 do
                        try
                            File.Delete path
                            remaining_retries <- -1
                        with err ->
                            remaining_retries <- remaining_retries - 1
                            Logging.Debug "Deleting file '%s' failed, retrying in 1s: %s" path err.Message
                            System.Threading.Thread.Sleep(1000)
                    if remaining_retries = 0 then Logging.Error "Deleting file '%s' failed, out of retries" path
                    return remaining_retries < 0
                }
        }

    let internal convert_song_folder (path: string, config: ConversionOptions, chart_db: ChartDatabase, user_db: UserDatabase) : Async<ConversionResult> =
        async {
            let results =
                Directory.EnumerateFiles path
                |> Seq.collect (
                    function
                    | ChartFile _ as file ->
                        let action = { Config = config; Source = file }
                        try
                            convert_chart_file action
                        with err ->
                            Logging.Error "Unhandled exception converting '%s': %O" file err
                            []
                    | _ -> []
                )
                |> Seq.map (
                    function
                    | Ok import ->
                        match Chart.check import.Chart with
                        | Ok chart -> Ok { import with Chart = chart }
                        | Error reason ->
                            Logging.Error "Conversion produced corrupt chart (%s): %s" path reason
                            Error (path, sprintf "Corrupt (%s)" reason)
                    | Error skipped_conversion -> Error skipped_conversion
                )
                |> List.ofSeq

            let filtered = filter_rates path results

            let mutable success_count = 0
            let charts =
                filtered
                |> List.choose (function Ok c -> success_count <- success_count + 1; Some c | _ -> None)
                |> List.map (fun c ->
                    let with_pruned_svs = { c.Chart with SV = cleaned_sv c.Chart.SV }
                    if with_pruned_svs.SV.Length < c.Chart.SV.Length then
                        let before_hash = Chart.hash c.Chart
                        let after_hash = Chart.hash with_pruned_svs
                        if before_hash <> after_hash then
                            match ChartDatabase.get_meta before_hash chart_db with
                            | Some chart_meta -> ChartDatabase.delete chart_meta chart_db
                            | None -> ()
                            if (UserDatabase.get_chart_data before_hash user_db).Scores.Length > 0 then
                                UserDatabase.transfer_scores before_hash after_hash user_db
                    { c with Chart = with_pruned_svs }
                )
            ChartDatabase.import charts chart_db
            return {
                ConvertedCharts = success_count
                SkippedCharts = filtered |> List.choose (function Error skipped -> Some skipped | _ -> None)
            }
        }

    let internal convert_pack_folder (path: string, config: ConversionOptions, chart_db: ChartDatabase, user_db: UserDatabase, progress: ProgressCallback) : Async<ConversionResult> =
        async {
            let mutable results = ConversionResult.Empty
            let song_folders =
                match config.ChangedAfter with
                | None -> Directory.EnumerateDirectories path
                | Some timestamp -> Directory.EnumerateDirectories path |> Seq.filter (fun path -> Directory.GetLastWriteTime path >= timestamp)
                |> Array.ofSeq

            for i, song_folder in Array.indexed song_folders do
                let! result = convert_song_folder(song_folder, config, chart_db, user_db)
                results <- ConversionResult.Combine result results
                progress <| Processing (i + 1, song_folders.Length)
            return results
        }

    let internal convert_folder_of_oszs (folder_of_oszs: string, chart_db: ChartDatabase, user_db: UserDatabase, progress: ProgressCallback) : Async<ConversionResult> =
        async {
            let config = ConversionOptions.Pack(Path.GetFileName folder_of_oszs, None, CopyAssetFiles)
            let mutable results = ConversionResult.Empty

            let files =
                Directory.EnumerateFiles folder_of_oszs
                |> Seq.filter (function ChartArchive ".osz" -> true | _ -> false)
                |> Array.ofSeq

            for i, osz in Array.indexed files do
                let extracted_folder = Path.ChangeExtension(osz, null).TrimEnd(' ', '.')
                if
                    Directory.Exists(extracted_folder)
                    && Directory.EnumerateFileSystemEntries(extracted_folder) |> Seq.isEmpty |> not
                then
                    Logging.Error "Can't extract osz to %s because that folder exists already" extracted_folder
                    results <- { results with SkippedCharts = (osz, "Extraction failed") :: results.SkippedCharts }
                else
                    try
                        ZipFile.ExtractToDirectory(osz, extracted_folder)
                        let! result = convert_song_folder(extracted_folder, config, chart_db, user_db)
                        delete_folder.Request(extracted_folder, ignore)
                        results <- ConversionResult.Combine result results
                    with err ->
                        Logging.Error "Unexpected error in '%s': %O" extracted_folder err
                        results <- { results with SkippedCharts = (extracted_folder, sprintf "Unexpected error: %s" err.Message) :: results.SkippedCharts }
                progress <| Processing (i + 1, files.Length)
            return results
        }

    let internal convert_stepmania_pack_zip (path: string, pack_name: string, chart_db: ChartDatabase, user_db: UserDatabase, progress: ProgressCallback) : Async<Result<ConversionResult, string>> =
        async {
            let dir = Path.ChangeExtension(path, null).TrimEnd(' ', '.')

            if
                Directory.Exists(dir)
                && Directory.EnumerateFileSystemEntries(dir) |> Seq.isEmpty |> not
            then
                return Error (sprintf "Target extraction folder (%s) already exists" dir)
            else
                ZipFile.ExtractToDirectory(path, dir)

                match dir with
                | FolderOfPacks ->
                    let mutable results = ConversionResult.Empty
                    for pack_folder in Directory.EnumerateDirectories dir do
                        let! result =
                            convert_pack_folder(
                                pack_folder,
                                ConversionOptions.EtternaPack(pack_name, None, CopyAssetFiles),
                                chart_db,
                                user_db,
                                progress
                            )
                        results <- ConversionResult.Combine result results

                    delete_folder.Request(dir, ignore)
                    return Ok results
                | _ ->
                    delete_folder.Request(dir, ignore)
                    return Error "Extracted zip does not match the usual structure for a StepMania pack"
        }

    let rec auto_detect_import (path: string, chart_db: ChartDatabase, user_db: UserDatabase, progress: ProgressCallback) : Async<Result<ConversionResult, string>> =
        async {
            try
                match File.GetAttributes path &&& FileAttributes.Directory |> int with
                | 0 ->
                    match path with
                    | ChartFile ext ->
                        let folder_name =
                            match ext with
                            | ".osu" -> "osu!"
                            | ".qua" -> "Quaver"
                            | _ -> "Singles"
                        let! result =
                            convert_song_folder(
                                Path.GetDirectoryName path,
                                ConversionOptions.Pack(folder_name, None, CopyAssetFiles),
                                chart_db,
                                user_db
                            )
                        log_conversion result
                        progress Complete
                        return Ok result
                    | ChartArchive _ ->
                        let dir = Path.ChangeExtension(path, null).TrimEnd(' ', '.')

                        if
                            Directory.Exists(dir)
                            && Directory.EnumerateFileSystemEntries(dir) |> Seq.isEmpty |> not
                        then
                            return Error (sprintf "Target extraction folder (%s) already exists" dir)
                        else
                            ZipFile.ExtractToDirectory(path, dir)
                            let! result = auto_detect_import (dir, chart_db, user_db, progress)
                            delete_folder.Request(dir, ignore)
                            return result
                    | _ ->
                        progress Faulted
                        return Error "File type not recognised as importable"
                | _ ->
                    match path with
                    | SongFolder ext ->
                        let folder_name =
                            match ext with
                            | ".osu" -> "osu!"
                            | ".qua" -> "Quaver"
                            | _ -> "Singles"

                        let! result =
                            convert_song_folder(
                                path,
                                ConversionOptions.Pack(folder_name, None, CopyAssetFiles),
                                chart_db,
                                user_db
                            )
                        log_conversion result
                        progress Complete
                        return Ok result
                    | FolderOfOszs ->
                        let! result =
                            convert_folder_of_oszs(
                                path,
                                chart_db,
                                user_db,
                                progress
                            )
                        log_conversion result
                        progress Complete
                        return Ok result
                    | PackFolder ->
                        let folder_name =
                            match Path.GetFileName path with
                            | "Songs" ->
                                if path |> Path.GetDirectoryName |> Path.GetFileName = "osu!" then
                                    "osu!"
                                elif path |> Path.GetDirectoryName |> Path.GetFileName = "Quaver" then
                                    "Quaver"
                                else
                                    "Songs"
                            | s -> s

                        let! result =
                            convert_pack_folder(
                                path,
                                ConversionOptions.Pack(folder_name, None, CopyAssetFiles),
                                chart_db,
                                user_db,
                                progress
                            )
                        log_conversion result
                        progress Complete
                        return Ok result
                    | FolderOfPacks ->
                        let mutable results = ConversionResult.Empty
                        let pack_folders = Directory.GetDirectories path

                        for i, pack_folder in Array.indexed pack_folders do
                            let pack_name = Path.GetFileName pack_folder

                            let! result =
                                convert_pack_folder(
                                    pack_folder,
                                    ConversionOptions.Pack(pack_name, None, CopyAssetFiles),
                                    chart_db,
                                    user_db,
                                    (fun p -> Nested (pack_name, i + 1, pack_folders.Length, p)) >> progress
                                )
                            results <- ConversionResult.Combine result results

                        log_conversion results
                        progress Complete
                        return Ok results
                    | _ ->
                        progress Faulted
                        return Error "No importable folder structure detected"
            with err ->
                Logging.Error "Unexpected exception while importing '%s': %O" path err
                progress Faulted
                return Error err.Message
        }