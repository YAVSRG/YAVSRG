namespace Interlude.Web.Server.API

open System.Collections.Generic
open NetCoreServer
open Percyqaz.Common
open Prelude
open Interlude.Web.Shared.API
open Interlude.Web.Server.API

module API =

    type Handler = string * Map<string, string array> * Map<string, string> * HttpResponse -> Async<unit>
    let handlers = Dictionary<(HttpMethod * string), Handler>()

    let inline add_endpoint route handle =
        handlers.Add( route, handle )

    do
        add_endpoint Auth.Discord.ROUTE Auth.Discord.handle
        add_endpoint Charts.Identify.ROUTE Charts.Identify.handle
        add_endpoint Health.HealthCheck.ROUTE Health.HealthCheck.handle

    let handle_request(method: HttpMethod, route: string, body: string, query_params: Map<string, string array>, header: Map<string, string>, response: HttpResponse) =
        async {
            if handlers.ContainsKey((method, route)) then
                try
                    let handler = handlers.[(method, route)]
                    do! handler(body, query_params, header, response)
                with err -> 
                    Logging.Error(sprintf "Error in %O %s: %O" method route err)
                    response.MakeErrorResponse(500, "Internal error") |> ignore
            else response.MakeErrorResponse(404, "Not found") |> ignore
        }