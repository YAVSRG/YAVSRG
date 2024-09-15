namespace Prelude.Charts.Conversions

open System.IO
open Percyqaz.Common
open Prelude.Charts
open Prelude.Charts.Formats.osu
open Prelude.Charts.Formats.StepMania
open Prelude.Charts.Formats.Quaver

[<AutoOpen>]
module Helpers =

    let convert_chart_file (action: ConversionAction) : Result<ImportChart, SkippedConversion> list =
        match Path.GetExtension(action.Source).ToLower() with
        | ".sm" ->
            match StepmaniaParser.parse_file action.Source with
            | Ok data ->

                StepMania_To_Interlude.convert data action

            | Error msg ->
                Logging.Debug(sprintf "Parse error in SM file %s" action.Source, msg)
                [ Error (action.Source, "Failed to parse this file") ]
        
        | ".qua" ->
            match QuaverChart.from_file action.Source with
            | Ok quaver_chart ->

                [ Quaver_To_Interlude.convert quaver_chart action ]

            | Error msg ->
                Logging.Debug(sprintf "Parse error in osu! file %s" action.Source, msg)
                [ Error (action.Source, "Failed to parse this file") ]

        | ".osu" ->
            match Beatmap.FromFile action.Source with
            | Ok beatmap when beatmap.General.Mode <> Gamemode.OSU_MANIA -> []
            | Ok beatmap ->

                [ Osu_To_Interlude.convert beatmap action ]

            | Error msg ->
                Logging.Debug(sprintf "Parse error in osu! file %s" action.Source, msg)
                [ Error (action.Source, "Failed to parse this file") ]

        | _ -> []
