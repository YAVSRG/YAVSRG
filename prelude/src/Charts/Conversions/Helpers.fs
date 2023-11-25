namespace Prelude.Charts.Conversions

open System.IO
open Percyqaz.Common
open Prelude.Charts
open Prelude.Charts.Formats.``osu!``
open Prelude.Charts.Formats.StepMania

[<AutoOpen>]
module Helpers =

    let convert_chart_file (action: ConversionAction) : Chart list =
        match Path.GetExtension(action.Source).ToLower() with
        | ".sm" ->
            let set_pack_source =
                if action.Config.StepmaniaPackId.IsSome then
                    fun chart ->
                        { chart with
                            Header =
                                { chart.Header with
                                    ChartSource = Stepmania action.Config.StepmaniaPackId.Value
                                }
                        }
                else
                    id

            match stepmania_data_from_file action.Source with
            | Ok data ->

                try
                    StepMania_To_Interlude.convert data action
                    |> List.map set_pack_source
                with
                | :? ConversionSkipException -> []
                | other_error ->
                    Logging.Debug(sprintf "Error converting %s" action.Source, other_error)
                    []

            | Error msg ->
                Logging.Debug(sprintf "Parse error in SM file %s" action.Source, msg)
                []

        | ".osu" ->
            match beatmap_from_file action.Source with
            | Ok beatmap ->

                try
                    [ Osu_To_Interlude.convert beatmap action ]
                with
                | :? ConversionSkipException -> []
                | other_error ->
                    Logging.Debug(sprintf "Error converting %s" action.Source, other_error)
                    []

            | Error msg ->
                Logging.Debug(sprintf "Parse error in osu! file %s" action.Source, msg)
                []

        | _ -> []