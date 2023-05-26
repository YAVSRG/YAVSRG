namespace Interlude.Web.Server.API.Health

open NetCoreServer
open Percyqaz.Json
open Interlude.Web.Shared.API

module HealthCheck =

    let ROUTE = (GET, "/health")

    [<Json.AutoCodec>]
    type Response = { Status: string }

    let mutable private request_counter = 0

    let handle (body: string, query_params: Map<string, string array>, headers: Map<string, string>) : Response =
        request_counter <- request_counter + 1
        { Status = sprintf "Everything will be ok. This endpoint has been called %i times since last restart." request_counter }