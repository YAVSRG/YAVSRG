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

    let [<Literal>] BEARER_PREFIX = "Bearer "
    let BEARER_PREFIX_LENGTH = BEARER_PREFIX.Length

    let authorize (headers: Map<string, string>) : int64 * User =

        let find_auth_header() =
            Map.tryFind "authorization" headers

        let validate_token(auth_header: string) : int64 * User =
            if auth_header.StartsWith(BEARER_PREFIX) then
                let bearer_token = auth_header.Substring(BEARER_PREFIX_LENGTH)

                match User.by_auth_token(bearer_token) with
                | Some(id, user) -> id, user
                | None -> raise AuthorizeFailedException

            else raise AuthorizeFailedException

        match find_auth_header() with
        | Some auth_header -> validate_token(auth_header)
        | None -> raise NotAuthorizedException

    let require_query_parameter (query_params: Map<string, string array>) (name: string) =
        if not (query_params.ContainsKey name) then
            raise (BadRequestException(Some(sprintf "'%s' is required" name)))