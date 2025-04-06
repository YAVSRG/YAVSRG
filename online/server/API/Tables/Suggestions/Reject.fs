namespace Interlude.Web.Server.API.Tables.Suggestions

open NetCoreServer
open Prelude
open Interlude.Web.Shared
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain.Core
open Interlude.Web.Server.Domain.Backbeat
open Interlude.Web.Server.Domain.Services

module Reject =

    open Tables.Suggestions.Reject

    let handle
        (
            body: string,
            query_params: Map<string, string array>,
            headers: Map<string, string>,
            response: HttpResponse
        ) =
        async {
            let user_id, user = authorize headers

            if not (user.Badges.Contains Badge.TABLE_EDITOR) then
                raise PermissionDeniedException

            match JSON.FromString body with
            | Error e -> raise (BadRequestException None)
            | Ok(request: Request) ->

            if not (Backbeat.Tables.exists request.TableId) then
                raise NotFoundException

            let chart_id = request.ChartId.ToUpper()

            if TableSuggestion.reject request.TableId chart_id user_id request.Reason then
                response.ReplyJson(true)
            else
                response.ReplyJson(false)
        }