namespace Interlude.Web.Server.API

open NetCoreServer
open Prelude
open Interlude.Web.Server.Domain.Core

[<AutoOpen>]
module Utils =

    exception NotAuthorizedException
    exception NotFoundException
    exception AuthorizeFailedException
    exception PermissionDeniedException
    exception BadRequestException of Message: string option

    let BEARER_LENGTH = "Bearer ".Length

    let authorize (header: Map<string, string>) =

        if header.ContainsKey("Authorization") then
            let auth_header = header.["Authorization"]
            if auth_header.Length > BEARER_LENGTH then
                match User.by_auth_token (header.["Authorization"].Substring(BEARER_LENGTH)) with
                | Some(id, user) -> id, user
                | None -> raise AuthorizeFailedException
            else raise AuthorizeFailedException

        else
            raise NotAuthorizedException

    let require_query_parameter (query_params: Map<string, string array>) (name: string) =
        if not (query_params.ContainsKey name) then
            raise (BadRequestException(Some(sprintf "'%s' is required" name)))