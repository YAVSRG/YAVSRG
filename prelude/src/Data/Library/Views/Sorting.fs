namespace Prelude.Data.Library

open System.Collections.Generic
open Percyqaz.Common
open Prelude
open Prelude.Gameplay
open Prelude.Data.Library.Caching
open Prelude.Data

type internal SortingTag = string * float32 * float
type SortMethod = CachedChart * LibraryViewContext -> SortingTag

module Sorting =

    let modes: IDictionary<string, SortMethod> =
        dict
            [
                "difficulty", fun (x, _) -> "", 0.0f, x.Physical
                "bpm", fun (x, _) -> "", (60000.0f<ms/minute> / snd x.BPM * 10.0f |> float32), x.Physical
                "title", fun (x, _) -> x.Title.ToLowerInvariant(), 0.0f, x.Physical
                "artist", fun (x, _) -> x.Artist.ToLowerInvariant(), 0.0f, x.Physical
                "creator", fun (x, _) -> x.Creator.ToLowerInvariant(), 0.0f, x.Physical
                "length", fun (x, _) -> "", (float32 x.Length), x.Physical
                "date_installed", fun (x, _) -> "", (Timestamp.from_datetime x.DateAdded |> float32), x.Physical
                "date_played", fun (x, ctx) -> 
                    let date_played =
                         match UserDatabase.get_chart_data_cached x.Hash ctx.UserDatabase with
                         | Some d -> d.LastPlayed |> float32
                         | None -> 0.0f
                    "", date_played, x.Physical
                "grade", fun (x, ctx) ->
                    match 
                        (UserDatabase.get_chart_data x.Hash ctx.UserDatabase).PersonalBests
                        |> Bests.ruleset_best_above ctx.RulesetId (_.Grade) ctx.Rate
                    with
                    | Some (i, _, _) -> "", float32 i, x.Physical
                    | None -> "", -2.0f, x.Physical
                "lamp", fun (x, ctx) ->
                    match 
                        (UserDatabase.get_chart_data x.Hash ctx.UserDatabase).PersonalBests
                        |> Bests.ruleset_best_above ctx.RulesetId (_.Lamp) ctx.Rate
                    with
                    | Some (i, _, _) -> "", float32 i, x.Physical
                    | None -> "", -2.0f, x.Physical
            ]