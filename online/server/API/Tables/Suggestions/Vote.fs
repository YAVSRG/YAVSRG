namespace Interlude.Web.Server.API.Tables.Suggestions

open NetCoreServer
open Prelude
open Interlude.Web.Shared
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain.Core
open Interlude.Web.Server.Domain.Backbeat
open Interlude.Web.Server.Domain.Services

module Vote =

    open Tables.Suggestions.Vote

    let handle
        (
            body: string,
            query_params: Map<string, string array>,
            headers: Map<string, string>,
            response: HttpResponse
        ) =
        async {
            let user_id, user = authorize headers

            match JSON.FromString body with
            | Error e -> raise (BadRequestException None)
            | Ok(request: Request) ->

            if not (Backbeat.Tables.exists request.TableId) then
                raise NotFoundException

            let chart_id = request.ChartId.ToUpper()
            let chart_is_known = (Backbeat.Charts.by_hash chart_id).IsSome

            let result =

                if user.Badges.Contains Badge.TABLE_EDITOR then
                    TableSuggestion.suggest_allow_reopening_rejected request.TableId chart_id user_id request.Level

                    if chart_is_known then
                        Response.Ok
                    else
                        Response.OkDetailsRequired
                else if TableSuggestion.suggest request.TableId chart_id user_id request.Level then
                    if chart_is_known then
                        Response.Ok
                    else
                        Response.OkDetailsRequired
                else
                    Response.Rejected

            response.ReplyJson(result)
        }