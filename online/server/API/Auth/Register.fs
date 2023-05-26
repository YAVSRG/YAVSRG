namespace Interlude.Web.Server.API.Auth

open System
open Percyqaz.Json
open Interlude.Web.Shared.API

module Register =

    let ROUTE = (GET, "/auth/register")

    [<Json.AutoCodec>]
    type Response = { Url: string }

    let handle (body: string, query_params: Map<string, string array>, headers: Map<string, string>) : Async<Response> =
        async {
            let state = Guid.NewGuid().ToString("N")
            let url = @"https://discord.com/api/oauth2/authorize?client_id=420320424199716864&redirect_uri=https%3A%2F%2Flocalhost%2Fauth%2Fdiscord&response_type=code&scope=identify"
            return { Url = sprintf "%s&state=%s" url state }
        }