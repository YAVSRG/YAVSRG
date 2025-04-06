namespace Interlude.Web.Server.API.Charts.Scores

open System
open NetCoreServer
open Interlude.Web.Shared
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
            let _, _ = authorize headers

            let chart_id = query_params.["chart"].[0].ToUpper()

            match Backbeat.Charts.by_hash chart_id with
            | Some _ ->
                let info = Scores.get_leaderboard_details chart_id

                let scores: Score array =
                    info
                    |> Array.map (fun (i, user, score, replay) ->
                        {
                            Username = user.Username
                            Rank = i + 1
                            Replay = replay.Data |> Convert.ToBase64String
                            Rate = score.Rate
                            Mods = score.Mods
                            Timestamp = score.TimePlayed
                        }
                    )

                response.ReplyJson<Response>({ Scores = scores })

            | None ->
                response.ReplyError(404, "Chart not leaderboarded") |> ignore
        }