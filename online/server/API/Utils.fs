namespace Interlude.Web.Server.API

open NetCoreServer
open Prelude
open Interlude.Web.Server.Domain

[<AutoOpen>]
module Utils =

    type HttpResponse with
        member this.ReplyJson<'T>(data: 'T) =
            this.MakeGetResponse(JSON.ToString data, "application/json") |> ignore

        member this.ReplyRedirect(url: string) =
            this.Clear().SetBegin(303).SetHeader("Location", url).SetBody() |> ignore

    exception NotAuthorizedException
    exception NotFoundException
    exception AuthorizeFailedException

    let authorize (header: Map<string, string>) =
        if header.ContainsKey("Authorization") then
            match User.by_auth_token (header.["Authorization"].Substring("Bearer ".Length)) with
            | Some(id, user) -> id, user
            | None -> raise AuthorizeFailedException
        else
            raise NotAuthorizedException
