namespace Interlude.Web.Server.API

open System.Collections.Generic
open NetCoreServer
open Percyqaz.Common
open Interlude.Web.Shared
open Interlude.Web.Shared.API
open Interlude.Web.Server

exception AuthorizeFailedException
exception PermissionDeniedException
exception NotAuthorizedException
exception NotFoundException
exception BadRequestException of Message: string option

type Handler = string * Map<string, string array> * Map<string, string> * HttpResponse -> Async<unit>

type API() =
    let handlers = Dictionary<Route, Handler>()

    member this.Register(route: Route, handler: Handler) : API =
        handlers.Add(route, handler)
        this

    member this.RegisterIf(condition: bool, route: Route, handler: Handler) : API =
        if condition then this.Register(route, handler) else this

    member this.Handle(route: Route, body: string, query_params: Map<string, string array>, headers: Map<string, string>, response: HttpResponse) : Async<unit> =
        async {
            if handlers.ContainsKey(route) then
                try
                    let handler = handlers.[route]
                    do! handler (body, query_params, headers, response)
                with
                | :? NotAuthorizedException ->
                    response.ReplyError(401, "Missing authorization token") |> ignore
                | :? NotFoundException ->
                    response.ReplyError(404, "Not found") |> ignore
                | :? AuthorizeFailedException ->
                    response.ReplyError(403, "Bad authorization token") |> ignore
                | :? PermissionDeniedException ->
                    response.ReplyError(403, "Permission denied") |> ignore
                | :? BadRequestException as err ->
                    response.ReplyError(400, Option.defaultValue "Bad request" err.Message)
                    |> ignore
                | err ->
                    let route_string =
                        let method, path = route
                        sprintf "%O %s" method path

                    Logging.Error "Unhandled exception in %s: %O" route_string err
                    Discord.debug_log (sprintf "Unhandled exception in %s\n%s" route_string (err.ToString()))
                    response.ReplyError(500, "Internal error") |> ignore
            else
                response.ReplyError(404, "Route not found") |> ignore
        }