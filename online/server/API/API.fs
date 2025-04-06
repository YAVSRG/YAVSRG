namespace Interlude.Web.Server.API

open System.Collections.Generic
open NetCoreServer
open Percyqaz.Common
open Interlude.Web.Shared
open Interlude.Web.Shared.API
open Interlude.Web.Shared.Requests
open Interlude.Web.Server
open Interlude.Web.Server.API

module API =

    type Handler = string * Map<string, string array> * Map<string, string> * HttpResponse -> Async<unit>
    let handlers = Dictionary<(HttpMethod * string), Handler>()

    let inline add_endpoint route handle = handlers.Add(route, handle)

    do
        add_endpoint Health.Status.ROUTE Health.Status.handle

        if not SECRETS.IsProduction then
            add_endpoint (GET, "/auth/dummy") Auth.Dummy.handle

        add_endpoint Auth.Discord.ROUTE Auth.Discord.handle

        add_endpoint Charts.Identify.ROUTE Charts.Identify.handle
        add_endpoint Charts.Add.ROUTE Charts.Add.handle
        add_endpoint Charts.Scores.Save.ROUTE Charts.Scores.Save.handle
        add_endpoint Charts.Scores.Leaderboard.ROUTE Charts.Scores.Leaderboard.handle

        add_endpoint Songs.Search.ROUTE Songs.Search.handle
        add_endpoint Songs.Scan.ROUTE Songs.Scan.handle
        add_endpoint Songs.Update.ROUTE Songs.Update.handle

        add_endpoint Tables.Records.ROUTE Tables.Records.handle
        add_endpoint Tables.Leaderboard.ROUTE Tables.Leaderboard.handle
        add_endpoint Tables.List.ROUTE Tables.List.handle
        add_endpoint Tables.Charts.ROUTE Tables.Charts.handle

        add_endpoint Tables.Suggestions.Vote.ROUTE Tables.Suggestions.Vote.handle
        add_endpoint Tables.Suggestions.List.ROUTE Tables.Suggestions.List.handle
        add_endpoint Tables.Suggestions.Missing.ROUTE Tables.Suggestions.Missing.handle
        add_endpoint Tables.Suggestions.Accept.ROUTE Tables.Suggestions.Accept.handle
        add_endpoint Tables.Suggestions.Reject.ROUTE Tables.Suggestions.Reject.handle

        add_endpoint Players.Online.ROUTE Players.Online.handle
        add_endpoint Players.Search.ROUTE Players.Search.handle

        add_endpoint Players.Profile.View.ROUTE Players.Profile.View.handle
        add_endpoint Players.Profile.Options.ROUTE Players.Profile.Options.handle

        add_endpoint Friends.List.ROUTE Friends.List.handle
        add_endpoint Friends.Add.ROUTE Friends.Add.handle
        add_endpoint Friends.Remove.ROUTE Friends.Remove.handle

        add_endpoint Stats.Sync.ROUTE Stats.Sync.handle
        add_endpoint Stats.Fetch.ROUTE Stats.Fetch.handle
        add_endpoint Stats.Leaderboard.XP.ROUTE Stats.Leaderboard.XP.handle
        add_endpoint Stats.Leaderboard.MonthlyXP.ROUTE Stats.Leaderboard.MonthlyXP.handle
        add_endpoint Stats.Leaderboard.Keymode.ROUTE Stats.Leaderboard.Keymode.handle
        add_endpoint Stats.Leaderboard.MonthlyKeymode.ROUTE Stats.Leaderboard.MonthlyKeymode.handle

    let handle_request
        (
            method: HttpMethod,
            route: string,
            body: string,
            query_params: Map<string, string array>,
            headers: Map<string, string>,
            response: HttpResponse
        ) =
        async {
            if handlers.ContainsKey((method, route)) then
                try
                    let handler = handlers.[(method, route)]
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
                    Logging.Error "Unhandled exception in %O %s: %O" method route err
                    Discord.debug_log (sprintf "Unhandled exception in %O %s\n%s" method route (err.ToString()))
                    response.ReplyError(500, "Internal error") |> ignore
            else
                response.ReplyError(404, "Route not found") |> ignore
        }