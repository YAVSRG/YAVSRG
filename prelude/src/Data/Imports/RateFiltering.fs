namespace Prelude.Data.Imports

open System
open System.Globalization
open System.Text.RegularExpressions
open Percyqaz.Common
open Prelude
open Prelude.Charts
open Prelude.Formats.Conversions

module RateFiltering =

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

    let apply (results: Result<ImportChart, SkippedConversion> list) : Result<ImportChart, SkippedConversion> list =

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
