namespace Prelude.Data.Library

open System.Collections.Generic
open Prelude.Data.UserDatabase

type internal SortingTag = string * float32 * float32
type SortMethod = ChartMeta * LibraryViewContext -> SortingTag

module Sorting =

    let modes: IDictionary<string, SortMethod> =
        dict
            [
                "difficulty", fun (chart_meta, _) -> "", 0.0f, chart_meta.Rating
                "bpm", fun (chart_meta, _) -> "", float32 chart_meta.BPM, chart_meta.Rating
                "title", fun (chart_meta, _) -> chart_meta.Title.ToLowerInvariant(), 0.0f, chart_meta.Rating
                "artist", fun (chart_meta, _) -> chart_meta.Artist.ToLowerInvariant(), 0.0f, chart_meta.Rating
                "creator", fun (chart_meta, _) -> chart_meta.Creator.ToLowerInvariant(), 0.0f, chart_meta.Rating
                "length", fun (chart_meta, _) -> "", float32 chart_meta.Length, chart_meta.Rating
                "date_installed", fun (chart_meta, _) -> "", float32 chart_meta.DateAdded, chart_meta.Rating
                "date_played",
                fun (chart_meta, ctx) ->
                    let date_played =
                        match ctx.Library.UserData.TryGetCachedChartData(chart_meta.Hash) with
                        | Some d -> d.LastPlayed |> float32
                        | None -> 0.0f

                    "", date_played, chart_meta.Rating
                "accuracy",
                fun (chart_meta, ctx) ->
                    let pbs = ctx.Library.UserData.GetChartData(chart_meta.Hash).PersonalBests
                    let ruleset_info = ctx.RulesetProvider(chart_meta)

                    match Bests.ruleset_best_above ruleset_info.Hash _.Accuracy ctx.Rate pbs with
                    | Some(acc, _, _) -> "", float32 acc, chart_meta.Rating
                    | None -> "", -2.0f, chart_meta.Rating
                "lamp",
                fun (chart_meta, ctx) ->
                    let pbs = ctx.Library.UserData.GetChartData(chart_meta.Hash).PersonalBests
                    let ruleset_info = ctx.RulesetProvider(chart_meta)

                    match Bests.ruleset_best_above ruleset_info.Hash _.Lamp ctx.Rate pbs with
                    | Some(i, _, _) ->
                        match Bests.ruleset_best_above ruleset_info.Hash _.Accuracy ctx.Rate pbs with
                        | Some(acc, _, _) -> "", float32 i, float32 acc
                        | None -> "", float32 i, 0.0f
                    | None -> "", -2.0f, chart_meta.Rating
            ]
