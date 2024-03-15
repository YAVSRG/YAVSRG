namespace Interlude.Web.Server.API.Charts.Scores

open NetCoreServer
open Prelude
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain.Services

module Save =

    open Charts.Scores.Save

    let handle
        (
            body: string,
            query_params: Map<string, string array>,
            headers: Map<string, string>,
            response: HttpResponse
        ) =
        async {
            let user_id, _ = authorize headers

            match JSON.FromString body with
            | Error e -> raise (BadRequestException None)
            | Ok(request: Request) -> // todo: basic clamp on how much data can be sent in one request (about 10kb?)

            let chart_id = request.ChartId.ToUpper()

            let timestamp =
                (System.DateTimeOffset.op_Implicit request.Timestamp).ToUnixTimeMilliseconds()

            match! Scores.submit (user_id, chart_id, request.Replay, request.Rate, request.Mods, timestamp) with
            | Scores.ScoreUploadOutcome.UploadFailed -> raise (BadRequestException None)
            | Scores.ScoreUploadOutcome.SongNotRecognised -> response.ReplyJson(None: Response)
            | Scores.ScoreUploadOutcome.Unrated -> response.ReplyJson(None: Response)
            | Scores.ScoreUploadOutcome.Rated(table_changes, leaderboard_changes) ->

                response.ReplyJson(
                    Some
                        {
                            // todo: these will be quite sick when set up but aren't set up for now
                            LeaderboardChanges =
                                leaderboard_changes
                                |> List.map (fun x ->
                                    {
                                        RulesetId = x.RulesetId
                                        OldRank = x.OldRank
                                        NewRank = x.NewRank
                                    }
                                )
                            TableChanges =
                                table_changes
                                |> List.map (fun x ->
                                    {
                                        Table = x.Table
                                        OldPosition = None
                                        NewPosition = (0, 0.0)
                                    }
                                )
                        }
                    : Response
                )
        }
