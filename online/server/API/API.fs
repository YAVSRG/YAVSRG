namespace Interlude.Web.Server.API

open System.Collections.Generic
open NetCoreServer
open Prelude
open Interlude.Web.Shared.API
open Interlude.Web.Server.API

module API =

    type Handler = string * Map<string, string array> * Map<string, string> -> string
    let handlers = Dictionary<(HttpMethod * string), Handler>()

    do
        handlers.Add( Health.HealthCheck.ROUTE, (Health.HealthCheck.handle >> JSON.ToString) )

    let handle_request(method: HttpMethod, route: string, body: string, query_params: Map<string, string array>, header: Map<string, string>, response: HttpResponse) =
        if handlers.ContainsKey((method, route)) then
            try
                let data = handlers.[(method, route)] (body, query_params, header)
                response.MakeGetResponse(data, "application/json")
            with err -> response.MakeErrorResponse(500, "Internal error")
        else response.MakeErrorResponse(404, "Not found")
        |> ignore