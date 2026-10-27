namespace Prelude.Data.Imports

open System.IO
open System.IO.Compression
open Percyqaz.Common
open Prelude.Charts
open Prelude.Formats.Conversions
open Prelude.Data
open Prelude.Data.Library
open Prelude.Formats.Osu
open Prelude.Formats.Quaver
open Prelude.Formats.StepMania

module FileSystemImports =

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

            let filtered = RateFiltering.apply(results)
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
        (path: string, config: ConversionOptions, library: Library, ctx: ITaskContext)
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
                ctx.Progress(i + 1, song_folders.Length)

            return results
        }

    let internal convert_folder_of_oszs
        (folder_of_oszs: string, library: Library, ctx: ITaskContext)
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

                ctx.Progress(i + 1, files.Length)

            return results
        }

    let internal convert_stepmania_pack_zip
        (path: string, pack_name: string, library: Library, ctx: ITaskContext)
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
                                ctx
                            )

                        results <- results.CombineWith(result)

                    FileSystemServices.delete_folder.Request(dir, ignore)
                    return Ok results
                | _ ->
                    FileSystemServices.delete_folder.Request(dir, ignore)
                    return Error "Extracted zip does not match the usual structure for a StepMania pack"
        }
