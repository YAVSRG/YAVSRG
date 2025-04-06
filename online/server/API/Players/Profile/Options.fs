namespace Interlude.Web.Server.API.Players.Profile

open NetCoreServer
open Prelude
open Interlude.Web.Shared
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain.Core

module Options =

    open Players.Profile.Options

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

            if
                user.Badges
                |> Seq.map Badge.badge_color
                |> Seq.concat
                |> Seq.contains request.Color
            then
                User.update_color (user_id, request.Color)
                response.ReplyJson(true)
            else
                response.ReplyJson(false)
        }