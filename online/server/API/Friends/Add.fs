namespace Interlude.Web.Server.API.Friends

open NetCoreServer
open Prelude
open Percyqaz.Common
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain.Objects

module Add =

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
            | Ok(request: Friends.Add.Request) ->

            match User.by_username request.User with
            | Some(id, user) ->
                Friends.add (user_id, id)
                response.ReplyJson(true)
            | None -> response.ReplyJson(false)
        }
