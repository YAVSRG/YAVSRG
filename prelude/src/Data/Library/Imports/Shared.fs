namespace Prelude.Data.Library.Imports

open System
open System.IO
open System.Text.RegularExpressions
open Percyqaz.Common
open Prelude.Charts
open Prelude.Charts.Formats.osu
open Prelude.Charts.Formats.Quaver
open Prelude.Charts.Formats.StepMania
open Prelude.Charts.Conversions

[<AutoOpen>]
module Shared =

    let OSU_SONG_FOLDER =
        Path.Combine(Environment.GetFolderPath Environment.SpecialFolder.LocalApplicationData, "osu!", "Songs")

    let STEPMANIA_PACK_FOLDER =
        Path.Combine(Path.GetPathRoot Environment.CurrentDirectory, "Games", "Stepmania 5", "Songs")

    let ETTERNA_PACK_FOLDER =
        Path.Combine(Path.GetPathRoot Environment.CurrentDirectory, "Games", "Etterna", "Songs")

    let QUAVER_SONG_FOLDER =
        Path.Combine(Environment.GetFolderPath Environment.SpecialFolder.ProgramFilesX86, "Steam", "steamapps", "common", "Quaver", "Songs")

    let log_conversion (result: ConversionResult) =
        let skipped = result.SkippedCharts.Length
        if skipped > 0 then
            let dump =
                result.SkippedCharts
                |> Seq.map (fun (path, reason) -> sprintf "%s -> %s" path reason)
                |> String.concat "\n "
            Logging.Info "Successful import of %i charts(s) also skipped %i file(s):\n %s" result.ConvertedCharts skipped dump
        else
            Logging.Info "Successfully imported %i charts(s)" result.ConvertedCharts

    let private RATE_REGEX =
        Regex(
            """((^|\s)([02][,.][0-9][0-9]?|1[,.]0[1-9]|1[,.][1-9][0-9]?)($|\s))|(x([02][,.][0-9][0-9]?|1[,.]0[1-9]|1[,.][1-9][0-9]?))|(([02][,.][0-9][0-9]?|1[,.]0[1-9]|1[,.][1-9][0-9]?)[x\]])"""
        )

    let detect_rate_mod (difficulty_name: string) : float32<rate> option =
        let m = RATE_REGEX.Match difficulty_name

        if m.Success then
            let r = m.Value.Trim([| ' '; 'x'; ']' |]).Replace(',', '.')

            match Single.TryParse r with
            | true, r -> Some (r * 1.0f<rate>)
            | false, _ -> None
        else
            None

    let filter_rates (path: string) (results: Result<ImportChart, string * string> list) : Result<ImportChart, string * string> list =
        results
        |> List.map (
            function
            | Ok import ->
                match detect_rate_mod import.Header.DiffName with
                | Some rate ->
                    let original =
                        results
                        |> List.tryPick (
                            function
                            | Ok { Chart = original; Header = header } ->
                                let original_duration = original.LastNote - original.FirstNote
                                let incoming_duration = import.Chart.LastNote - import.Chart.FirstNote
                                if
                                    original.Notes.Length = import.Chart.Notes.Length &&
                                    abs (incoming_duration * float32 rate - original_duration) < 5.0f<ms>
                                then
                                    match import.Header.Origins |> Set.toSeq |> Seq.tryHead with
                                    | Some (ChartOrigin.Osu (md5, set_id, map_id, _, _)) ->
                                        Some (header, ChartOrigin.Osu (md5, set_id, map_id, rate, import.Chart.FirstNote))
                                    | _ -> None
                                else None
                            | _ -> None
                        )
                    match original with
                    | Some (h, alt_rate_origin) ->
                        h.Origins <- h.Origins.Add alt_rate_origin
                        Error (path, sprintf "Skipping %.2fx rate of another map" rate)
                    | None -> Ok import
                | None -> Ok import
            | Error skipped_conversion -> Error skipped_conversion
        )

    let convert_chart_file (action: ConversionAction) : Result<ImportChart, SkippedConversion> list =
        match Path.GetExtension(action.Source).ToLower() with
        | ".sm" ->
            match StepMania.FromFile action.Source with
            | Ok data ->

                StepMania_To_Interlude.convert data action

            | Error msg ->
                Logging.Debug "Parse error in SM file %s: %O" action.Source msg
                [ Error (action.Source, "Failed to parse this file") ]

        | ".qua" ->
            match QuaverChart.FromFile action.Source with
            | Ok quaver_chart ->

                [ Quaver_To_Interlude.convert quaver_chart action ]

            | Error msg ->
                Logging.Debug "Parse error in osu! file %s: %O" action.Source msg
                [ Error (action.Source, "Failed to parse this file") ]

        | ".osu" ->
            match Beatmap.FromFile action.Source with
            | Ok beatmap when beatmap.General.Mode <> Gamemode.OSU_MANIA -> []
            | Ok beatmap ->

                [ Osu_To_Interlude.convert beatmap action ]

            | Error msg ->
                Logging.Debug "Parse error in osu! file %s: %O" action.Source msg
                [ Error (action.Source, "Failed to parse this file") ]

        | _ -> []