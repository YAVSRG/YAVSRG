namespace Prelude.Data.Library

open System.Collections.Generic
open Prelude.Data.User

type internal SortingTag = string * float32 * float32
type SortMethod = ChartMeta * LibraryViewContext -> SortingTag

module Sorting =

    let modes: IDictionary<string, SortMethod> =
        dict
            [
                "difficulty", fun (x, _) -> "", 0.0f, x.Rating
                "bpm", fun (x, _) -> "", float32 x.BPM, x.Rating
                "title", fun (x, _) -> x.Title.ToLowerInvariant(), 0.0f, x.Rating
                "artist", fun (x, _) -> x.Artist.ToLowerInvariant(), 0.0f, x.Rating
                "creator", fun (x, _) -> x.Creator.ToLowerInvariant(), 0.0f, x.Rating
                "length", fun (x, _) -> "", float32 x.Length, x.Rating
                "date_installed", fun (x, _) -> "", float32 x.DateAdded, x.Rating
                "date_played", fun (x, ctx) ->
                    let date_played =
                         match UserDatabase.get_chart_data_cached x.Hash ctx.UserDatabase with
                         | Some d -> d.LastPlayed |> float32
                         | None -> 0.0f
                    "", date_played, x.Rating
                "grade", fun (x, ctx) ->
                    match
                        (UserDatabase.get_chart_data x.Hash ctx.UserDatabase).PersonalBests
                        |> Bests.ruleset_best_above ctx.RulesetId (_.Grade) ctx.Rate
                    with
                    | Some (i, _, _) -> "", float32 i, x.Rating
                    | None -> "", -2.0f, x.Rating
                "lamp", fun (x, ctx) ->
                    match
                        (UserDatabase.get_chart_data x.Hash ctx.UserDatabase).PersonalBests
                        |> Bests.ruleset_best_above ctx.RulesetId (_.Lamp) ctx.Rate
                    with
                    | Some (i, _, _) -> "", float32 i, x.Rating
                    | None -> "", -2.0f, x.Rating
            ]