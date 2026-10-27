namespace Prelude.Data.Imports

open System
open System.Globalization
open System.IO
open System.IO.Compression
open System.Text.RegularExpressions
open Percyqaz.Common
open Prelude
open Prelude.Charts
open Prelude.Formats.Conversions
open Prelude.Data
open Prelude.Data.Library
open Prelude.Formats.Osu
open Prelude.Formats.Quaver
open Prelude.Formats.StepMania

module FileSystemImports =

    let private RATE_REGEX =
        Regex(
            """((^|\s)([023][,.][0-9][0-9]?|1[,.]0[1-9]|1[,.][1-9][0-9]?)($|\s))|(x([023][,.][0-9][0-9]?|1[,.]0[1-9]|1[,.][1-9][0-9]?))|(([023][,.][0-9][0-9]?|1[,.]0[1-9]|1[,.][1-9][0-9]?)[x\]])"""
        )

    let detect_rate_mod (difficulty_name: string) : Rate option =
        let m = RATE_REGEX.Match(difficulty_name)

        if m.Success then
            let r = m.Value.Trim([| ' '; 'x'; ']' |]).Replace(',', '.')

            match Single.TryParse(r, CultureInfo.InvariantCulture) with
            | true, r when r >= float32 LOWEST_SUPPORTED_RATE && r <= float32 HIGHEST_SUPPORTED_RATE ->
                Some(r * 1.0f<rate>)
            | _ -> None
        else
            None

    let filter_rates
        (results: Result<ImportChart, SkippedConversion> list)
        : Result<ImportChart, SkippedConversion> list =

        let find_matching_1x (import: ImportChart, rate: Rate) =
            results
            |> List.tryPick(
                function
                | Ok { Chart = original; Header = header } ->
                    let original_duration = original.LastNote - original.FirstNote
                    let incoming_duration = import.Chart.LastNote - import.Chart.FirstNote
                    let relative_rate = original_duration / incoming_duration * 1.0f<rate>

                    if
                        abs(rate - relative_rate) < 0.025f<rate>
                        && Chart.notecount original = Chart.notecount import.Chart
                    then
                        match import.Header.Origins |> Set.toSeq |> Seq.tryHead with
                        | Some(ChartOrigin.Osu osu) ->
                            Some(
                                header,
                                ChartOrigin.Osu(
                                    { osu with SourceRate = rate; FirstNoteOffset = import.Chart.FirstNote }
                                )
                            )
                        | _ -> None
                    else
                        None
                | _ -> None
            )

        results
        |> List.map(
            function
            | Ok import ->
                match detect_rate_mod import.Header.DiffName with
                | Some rate ->
                    match find_matching_1x(import, rate) with
                    | Some(h, alt_rate_origin) ->
                        h.Origins <- h.Origins.Add(alt_rate_origin)

                        Error
                            {
                                OriginalPath = import.LoadedFromPath
                                Reason = sprintf "Skipping %.2fx rate of another map" rate
                            }
                    | None ->
                        Logging.Debug
                            "%s: Looks like %.2fx rate, but didn't find a 1.0x version in the same folder, so keeping it"
                            import.LoadedFromPath
                            rate

                        Ok import
                | None -> Ok import
            | Error skipped_conversion -> Error skipped_conversion
        )

    let convert_chart_file (ctx: ConversionContext) : Result<ImportChart, SkippedConversion> list =
        let inline parse_error (message: string) =
            [
                Error { OriginalPath = ctx.OriginalPath; Reason = sprintf "Failed to parse this file: %s" message }
            ]

        match Path.GetExtension(ctx.OriginalPath).ToLower() with
        | ".sm" ->
            match StepMania.FromFile(ctx.OriginalPath) with
            | Ok data -> StepManiaConverter.convert data ctx
            | Error msg -> parse_error(msg)

        | ".qua" ->
            match QuaverChart.FromFile(ctx.OriginalPath) with
            | Ok quaver_chart -> [ QuaverConverter.convert quaver_chart ctx ]
            | Error msg -> parse_error(msg)

        | ".osu" ->
            match Beatmap.TryReadFromFile(ctx.OriginalPath) with
            | Ok beatmap when beatmap.General.Mode <> Gamemode.OSU_MANIA -> []
            | Ok beatmap -> [ OsuConverter.convert beatmap ctx ]
            | Error msg -> parse_error(msg)

        | _ -> []

    let internal convert_song_folder
        (path: string, config: ConversionOptions, library: Library)
        : Async<ConversionResult> =
        async {
            let results =
                Directory.EnumerateFiles(path)
                |> Seq.collect(
                    function
                    | ChartFile _ as file ->
                        let ctx = { Options = config; OriginalPath = file }
                        convert_chart_file(ctx)
                    | _ -> []
                )
                |> Seq.map(
                    function
                    | Ok import ->
                        match import.Chart.CheckForErrors() with
                        | Ok chart -> Ok { import with Chart = chart }
                        | Error reason ->
                            Error
                                {
                                    OriginalPath = import.LoadedFromPath
                                    Reason = sprintf "Conversion produced corrupt chart (%s)" reason
                                }
                    | Error skipped_conversion -> Error skipped_conversion
                )
                |> List.ofSeq

            let filtered = filter_rates results

            let mutable success_count = 0

            let charts =
                filtered
                |> List.choose(
                    function
                    | Ok c ->
                        success_count <- success_count + 1
                        Some c
                    | _ -> None
                )

            return
                {
                    ConvertedCharts = success_count
                    FirstConvertedChart = library.Charts.Import(charts) |> Array.tryHead
                    SkippedCharts =
                        filtered
                        |> List.choose(
                            function
                            | Error skipped -> Some skipped
                            | _ -> None
                        )
                }
        }

    let internal convert_pack_folder
        (path: string, config: ConversionOptions, library: Library, progress: ProgressCallback)
        : Async<ConversionResult> =
        async {
            let mutable results = ConversionResult.Empty

            let song_folders =
                match config.ChangedAfter with
                | None -> Directory.EnumerateDirectories(path)
                | Some timestamp ->
                    Directory.EnumerateDirectories(path)
                    |> Seq.filter(fun path -> Directory.GetLastWriteTime(path) >= timestamp)
                |> Array.ofSeq

            for i, song_folder in Array.indexed song_folders do
                let! result = convert_song_folder(song_folder, config, library)
                results <- results.CombineWith(result)
                progress(Processing(i + 1, song_folders.Length))

            return results
        }

    let internal convert_folder_of_oszs
        (folder_of_oszs: string, library: Library, progress: ProgressCallback)
        : Async<ConversionResult> =
        async {
            let config = ConversionOptions.Pack(Path.GetFileName(folder_of_oszs), None, Copy)
            let mutable results = ConversionResult.Empty

            let files =
                Directory.EnumerateFiles(folder_of_oszs)
                |> Seq.filter(
                    function
                    | ChartArchive ".osz" -> true
                    | _ -> false
                )
                |> Array.ofSeq

            for i, osz in Array.indexed files do
                let extracted_folder = Path.ChangeExtension(osz, null).TrimEnd(' ', '.')

                if
                    Directory.Exists(extracted_folder)
                    && Directory.EnumerateFileSystemEntries(extracted_folder) |> Seq.isEmpty |> not
                then
                    Logging.Error "Can't extract osz to %s because that folder exists already" extracted_folder

                    results <-
                        { results with
                            SkippedCharts =
                                { OriginalPath = osz; Reason = "Extraction failed" } :: results.SkippedCharts
                        }
                else
                    try
                        ZipFile.ExtractToDirectory(osz, extracted_folder)
                        let! result = convert_song_folder(extracted_folder, config, library)
                        FileSystemServices.delete_folder.Request(extracted_folder, ignore)
                        results <- results.CombineWith(result)
                    with err ->
                        Logging.Error "Unexpected error in '%s': %O" extracted_folder err

                        results <-
                            { results with
                                SkippedCharts =
                                    {
                                        OriginalPath = extracted_folder
                                        Reason = sprintf "Unexpected error: %s" err.Message
                                    }
                                    :: results.SkippedCharts
                            }

                progress(Processing(i + 1, files.Length))

            return results
        }

    let internal convert_stepmania_pack_zip
        (path: string, pack_name: string, library: Library, progress: ProgressCallback)
        : Async<Result<ConversionResult, string>> =
        async {
            let dir = Path.ChangeExtension(path, null).TrimEnd(' ', '.')

            if Directory.Exists(dir) && Directory.EnumerateFileSystemEntries(dir) |> Seq.isEmpty |> not then
                return Error(sprintf "Target extraction folder (%s) already exists" dir)
            else
                ZipFile.ExtractToDirectory(path, dir)

                match dir with
                | FolderOfPacks ->
                    let mutable results = ConversionResult.Empty

                    for pack_folder in Directory.EnumerateDirectories(dir) do
                        let! result =
                            convert_pack_folder(
                                pack_folder,
                                ConversionOptions.EtternaPack(pack_name, None, Copy),
                                library,
                                progress
                            )

                        results <- results.CombineWith(result)

                    FileSystemServices.delete_folder.Request(dir, ignore)
                    return Ok results
                | _ ->
                    FileSystemServices.delete_folder.Request(dir, ignore)
                    return Error "Extracted zip does not match the usual structure for a StepMania pack"
        }

    let rec auto_detect_import
        (path: string, library: Library, progress: ProgressCallback)
        : Async<Result<ConversionResult, string>> =
        async {
            try
                match File.GetAttributes(path) &&& FileAttributes.Directory |> int with
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
                                Path.GetDirectoryName(path),
                                ConversionOptions.Pack(folder_name, None, Copy),
                                library
                            )

                        result.LogInfo()
                        return Ok result
                    | ChartArchive _ ->
                        let dir = Path.ChangeExtension(path, null).TrimEnd(' ', '.')

                        if Directory.Exists(dir) && Directory.EnumerateFileSystemEntries(dir) |> Seq.isEmpty |> not then
                            return Error(sprintf "Target extraction folder (%s) already exists" dir)
                        else
                            ZipFile.ExtractToDirectory(path, dir)
                            let! result = auto_detect_import(dir, library, progress)
                            FileSystemServices.delete_folder.Request(dir, ignore)
                            return result
                    | _ -> return Error "File type not recognised as importable"
                | _ ->
                    match path with
                    | SongFolder ext ->
                        let folder_name =
                            match ext with
                            | ".osu" -> "osu!"
                            | ".qua" -> "Quaver"
                            | _ -> "Singles"

                        let! result =
                            convert_song_folder(path, ConversionOptions.Pack(folder_name, None, Copy), library)

                        result.LogInfo()
                        return Ok result
                    | FolderOfOszs ->
                        let! result = convert_folder_of_oszs(path, library, progress)
                        result.LogInfo()
                        return Ok result
                    | PackFolder ->
                        let folder_name =
                            match Path.GetFileName(path) with
                            | "Songs" ->
                                if path |> Path.GetDirectoryName |> Path.GetFileName = "osu!" then "osu!"
                                elif path |> Path.GetDirectoryName |> Path.GetFileName = "Quaver" then "Quaver"
                                else "Songs"
                            | s -> s

                        let! result =
                            convert_pack_folder(
                                path,
                                ConversionOptions.Pack(folder_name, None, Copy),
                                library,
                                progress
                            )

                        result.LogInfo()
                        return Ok result
                    | FolderOfPacks ->
                        let mutable results = ConversionResult.Empty
                        let pack_folders = Directory.GetDirectories(path)

                        for i, pack_folder in Array.indexed pack_folders do
                            let pack_name = Path.GetFileName(pack_folder)

                            let! result =
                                convert_pack_folder(
                                    pack_folder,
                                    ConversionOptions.Pack(pack_name, None, Copy),
                                    library,
                                    (fun p -> Nested(pack_name, i + 1, pack_folders.Length, p)) >> progress
                                )

                            results <- results.CombineWith(result)

                        results.LogInfo()
                        return Ok results
                    | _ -> return Error "No importable folder structure detected"
            with err ->
                Logging.Error "Unexpected exception while importing '%s': %O" path err
                return Error err.Message
        }
