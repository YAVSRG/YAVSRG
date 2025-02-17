namespace Prelude.Data.Library.Imports

open System.IO
open System.IO.Compression
open Percyqaz.Common
open Prelude.Charts
open Prelude.Formats
open Prelude.Data.Library

module Imports =

    let delete_folder =
        { new Async.Service<string, bool>() with
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
        { new Async.Service<string, bool>() with
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

    let convert_song_folder =
        { new Async.Service<string * ConversionOptions * Library, ConversionResult>() with
            override this.Handle((path, config, { Charts = chart_db })) =
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
                    let charts = filtered |> List.choose (function Ok c -> success_count <- success_count + 1; Some c | _ -> None)
                    ChartDatabase.import charts chart_db
                    return {
                        ConvertedCharts = success_count
                        SkippedCharts = filtered |> List.choose (function Error skipped -> Some skipped | _ -> None)
                    }
                }
        }

    let convert_pack_folder =
        { new Async.Service<string * ConversionOptions * Library, ConversionResult>() with
            override this.Handle((path, config, library)) =
                async {
                    let mutable results = ConversionResult.Empty
                    for song_folder in
                        (Directory.EnumerateDirectories path
                         |> match config.ChangedAfter with
                            | None -> id
                            | Some timestamp -> Seq.filter (fun path -> Directory.GetLastWriteTime path >= timestamp)) do
                        let! result = convert_song_folder.RequestAsync(song_folder, config, library)
                        results <- ConversionResult.Combine result results
                    return results
                }
        }

    let convert_folder_of_oszs =
        { new Async.Service<string * Library, ConversionResult>() with
            override this.Handle((folder_of_oszs, library)) =
                async {
                    let config =
                        { ConversionOptions.Default with
                            MoveAssets = true
                            PackName = Path.GetFileName folder_of_oszs
                        }
                    let mutable results = ConversionResult.Empty
                    for osz in Directory.EnumerateFiles folder_of_oszs do
                        match osz with
                        | ChartArchive ".osz" ->
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
                                    let! result = convert_song_folder.RequestAsync(extracted_folder, config, library)
                                    Directory.Delete(extracted_folder, true)
                                    results <- ConversionResult.Combine result results
                                with err ->
                                    Logging.Error "Unexpected error in '%s': %O" extracted_folder err
                                    results <- { results with SkippedCharts = (extracted_folder, sprintf "Unexpected error: %s" err.Message) :: results.SkippedCharts }
                        | _ -> ()
                    return results
                }
        }

    let convert_stepmania_pack_zip =
        { new Async.Service<string * string * Library, ConversionResult option>() with
            override this.Handle((path, pack_name, library)) =
                async {
                    let dir = Path.ChangeExtension(path, null).TrimEnd(' ', '.')

                    if
                        Directory.Exists(dir)
                        && Directory.EnumerateFileSystemEntries(dir) |> Seq.isEmpty |> not
                    then
                        Logging.Error "Can't extract zip to %s because that folder exists already" dir
                        return None
                    else
                        ZipFile.ExtractToDirectory(path, dir)

                        match dir with
                        | FolderOfPacks ->
                            let mutable results = ConversionResult.Empty
                            for pack_folder in Directory.EnumerateDirectories dir do
                                let! result =
                                    convert_pack_folder.RequestAsync(
                                        pack_folder,
                                        { ConversionOptions.Default with
                                            EtternaPackName = Some pack_name
                                            PackName = Path.GetFileName pack_folder
                                            MoveAssets = true
                                        },
                                        library
                                    )
                                results <- ConversionResult.Combine result results

                            delete_folder.Request(dir, ignore)
                            return Some results
                        | _ ->
                            Logging.Warn "'%s': Extracted zip does not match the usual structure for a StepMania pack" dir
                            delete_folder.Request(dir, ignore)
                            return None
                }
        }

    let rec private auto_detect_import (path: string, move_assets: bool, library: Library) : Async<ConversionResult option> =
        async {
            match File.GetAttributes path &&& FileAttributes.Directory |> int with
            | 0 ->
                match path with
                | ChartFile ext ->
                    let! result =
                        convert_song_folder.RequestAsync(
                            Path.GetDirectoryName path,
                            { ConversionOptions.Default with
                                PackName =
                                    if ext = ".osu" then "osu!"
                                    elif ext = ".qua" then "Quaver"
                                    else "Singles"
                            },
                            library
                        )
                    log_conversion result
                    return Some result
                | ChartArchive ext ->
                    let dir = Path.ChangeExtension(path, null).TrimEnd(' ', '.')

                    if
                        Directory.Exists(dir)
                        && Directory.EnumerateFileSystemEntries(dir) |> Seq.isEmpty |> not
                    then
                        Logging.Error "Can't extract %s to %s because that folder exists already" ext dir
                        return None
                    else
                        ZipFile.ExtractToDirectory(path, dir)
                        let! result = auto_detect_import (dir, true, library)
                        delete_folder.Request(dir, ignore)
                        return result
                | _ ->
                    Logging.Warn "%s: Unrecognised file for import" path
                    return None
            | _ ->
                match path with
                | SongFolder ext ->
                    let! result =
                        convert_song_folder.RequestAsync(
                            path,
                            { ConversionOptions.Default with
                                MoveAssets = move_assets
                                PackName =
                                    if ext = ".osu" then "osu!"
                                    elif ext = ".qua" then "Quaver"
                                    else "Singles"
                            },
                            library
                        )
                    log_conversion result
                    return Some result
                | FolderOfOszs ->
                    let! result =
                        convert_folder_of_oszs.RequestAsync(
                            path,
                            library
                        )
                    log_conversion result
                    return Some result
                | PackFolder ->
                    let packname =
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
                        convert_pack_folder.RequestAsync(
                            path,
                            { ConversionOptions.Default with
                                PackName = packname
                                MoveAssets = move_assets
                            },
                            library
                        )
                    log_conversion result
                    return Some result
                | FolderOfPacks ->
                    let mutable results = ConversionResult.Empty
                    for pack_folder in Directory.EnumerateDirectories path do
                        let! result =
                            convert_pack_folder.RequestAsync(
                                pack_folder,
                                { ConversionOptions.Default with
                                    PackName = Path.GetFileName pack_folder
                                    MoveAssets = move_assets
                                },
                                library
                            )
                        results <- ConversionResult.Combine result results

                    log_conversion results
                    return Some results
                | _ ->
                    Logging.Warn "%s: No importable folder structure detected" path
                    return None
        }

    let auto_convert =
        { new Async.Service<string * bool * Library, ConversionResult option>() with
            override this.Handle((path, move_assets, library)) =
                auto_detect_import (path, move_assets, library)
        }