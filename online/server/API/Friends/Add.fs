namespace Interlude.Web.Server.API.Friends

open NetCoreServer
open Prelude
open Interlude.Web.Shared
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain.Core

module Add =

    open Friends.Add

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
            | Ok(request: Request) ->

            match User.by_username request.User with
            | Some(id, user) ->
                match Friends.add (user_id, id) with
                | Ok() -> response.ReplyJson(true)
                | Error reason -> response.ReplyJson(false) // todo: get this reason back to the user
            | None -> response.ReplyJson(false)
        }