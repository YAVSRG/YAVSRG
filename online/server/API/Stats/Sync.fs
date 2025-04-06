namespace Interlude.Web.Server.API.Stats

open NetCoreServer
open Percyqaz.Common
open Prelude
open Interlude.Web.Shared
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain.Services

module Sync =

    open Stats.Sync

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

            match Stats.sync user_id request with
            | Ok() -> response.ReplyJson(true)
            | Error reason ->
                Logging.Error "Error syncing stats for user #%i '%s': %s" user_id user.Username reason
                response.ReplyJson(false)
        }