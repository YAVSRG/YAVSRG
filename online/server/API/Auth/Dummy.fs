namespace Interlude.Web.Server.API.Auth

open NetCoreServer
open Interlude.Web.Shared
open Interlude.Web.Server
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain.Core

module Dummy =

    let handle
        (
            body: string,
            query_params: Map<string, string array>,
            headers: Map<string, string>,
            response: HttpResponse
        ) =
        async {
            if SECRETS.IsProduction then
                failwith "This code should be unreachable in production!"

            require_query_parameter query_params "username"

            let username = query_params.["username"].[0]

            let token =
                match User.by_username username with
                | Some(id, u) -> u.AuthToken
                | None ->
                    let user = User.create (username, 0uL)
                    User.save_new user |> ignore
                    user.AuthToken

            response.ReplyJson token
        }