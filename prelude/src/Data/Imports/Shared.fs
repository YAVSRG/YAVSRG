namespace Prelude.Data.Imports

open System
open System.Globalization
open System.IO
open System.Text.RegularExpressions
open Percyqaz.Common
open Prelude
open Prelude.Charts
open Prelude.Formats.Osu
open Prelude.Formats.Quaver
open Prelude.Formats.StepMania
open Prelude.Formats.Conversions

[<AutoOpen>]
module Shared =

    let log_conversion (result: ConversionResult) =
        let skipped = result.SkippedCharts.Length

        if skipped > 0 then
            let dump = result.SkippedCharts |> Seq.map _.ToString() |> String.concat "\n "

            Logging.Info
                "Successful import of %i chart(s) also skipped %i file(s):\n %s"
                result.ConvertedCharts
                skipped
                dump
        else
            Logging.Info "Successfully imported %i chart(s)" result.ConvertedCharts

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

    let import_queue =
        { new Async.Queue<Async<Result<ConversionResult, string>>, Result<ConversionResult, string>>() with
            override this.Handle
                (task: Async<Result<ConversionResult, string>>)
                : Async<Result<ConversionResult, string>> =
                task
        }

    let import_queue_new =
        { new Async.Queue<Async<unit>, unit>() with
            override this.Handle(task: Async<unit>) : Async<unit> = task
        }

    let general_task_queue =
        { new Async.Queue<Async<unit>, unit>() with
            override this.Handle(task: Async<unit>) : Async<unit> = task
        }
