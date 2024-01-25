namespace Interlude.Web.Server.API.Charts.Scores

open NetCoreServer
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain.Objects

module Leaderboard =

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
                if ruleset_id <> Score.PRIMARY_RULESET && not (Leaderboard.exists chart_id ruleset_id) then
                    Score.PRIMARY_RULESET
                else
                    ruleset_id

            if Leaderboard.exists chart_id ruleset_id then

                let info = Score.get_leaderboard chart_id ruleset_id

                printfn "%A" info

                // todo: score service to pull leaderboard, username info, replay info

                //let scores: Charts.Scores.Leaderboard.Score array =
                //    info
                //    |> Array.indexed
                //    |> Array.choose (
                //        function
                //        | i, (Some username, Some found_score) ->
                //            Some
                //                {
                //                    Username = username
                //                    Rank = i + 1
                //                    Replay = found_score.Replay
                //                    Rate = found_score.Rate
                //                    Mods = found_score.Mods
                //                    Timestamp =
                //                        System.DateTimeOffset
                //                            .FromUnixTimeMilliseconds(found_score.Timestamp)
                //                            .UtcDateTime
                //                }
                //        | _ -> None
                //    )

                response.ReplyJson({ Scores = [||]; RulesetId = ruleset_id }: Charts.Scores.Leaderboard.Response)

            else
                response.MakeErrorResponse(404) |> ignore
        }
