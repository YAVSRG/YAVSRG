namespace Interlude.Web.Server.API.Charts.Scores

open NetCoreServer
open Prelude
open Interlude.Web.Shared
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

            match! Scores.submit (user_id, chart_id, request.Replay, request.Rate, request.Mods, request.Timestamp) with
            | Scores.ScoreUploadOutcome.Failed -> raise (BadRequestException None)
            | Scores.ScoreUploadOutcome.Unranked -> response.ReplyJson<Response>(None)
            | Scores.ScoreUploadOutcome.Ranked(new_position) ->
                response.ReplyJson<Response>(Some { LeaderboardPosition = new_position })
        }