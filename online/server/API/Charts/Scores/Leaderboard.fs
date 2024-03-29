﻿namespace Interlude.Web.Server.API.Charts.Scores

open System
open NetCoreServer
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain.Core
open Interlude.Web.Server.Domain.Services

module Leaderboard =

    open Charts.Scores.Leaderboard

    let handle
        (
            body: string,
            query_params: Map<string, string array>,
            headers: Map<string, string>,
            response: HttpResponse
        ) =
        async {
            require_query_parameter query_params "chart"
            require_query_parameter query_params "ruleset"
            let _, _ = authorize headers

            let chart_id = query_params.["chart"].[0].ToUpper()
            let ruleset_id = query_params.["ruleset"].[0]

            let ruleset_id =
                if
                    ruleset_id <> Score.PRIMARY_RULESET
                    && not (Leaderboard.exists chart_id ruleset_id)
                then
                    Score.PRIMARY_RULESET
                else
                    ruleset_id

            if Leaderboard.exists chart_id ruleset_id then

                let info = Scores.get_leaderboard_details chart_id ruleset_id

                let scores: Score array =
                    info
                    |> Array.map (fun (i, user, score, replay) ->
                        {
                            Username = user.Username
                            Rank = i + 1
                            Replay = replay.Data |> Convert.ToBase64String
                            Rate = score.Rate
                            Mods = score.Mods
                            Timestamp = DateTimeOffset.FromUnixTimeMilliseconds(score.TimePlayed).UtcDateTime
                        }
                    )

                response.ReplyJson(
                    {
                        Scores = scores
                        RulesetId = ruleset_id
                    }
                    : Response
                )

            else
                response.MakeErrorResponse(404) |> ignore
        }
