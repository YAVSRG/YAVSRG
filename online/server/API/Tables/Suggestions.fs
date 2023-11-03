namespace Interlude.Web.Server.API.Tables

open NetCoreServer
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain

module Suggestions =

    let handle
        (
            body: string,
            query_params: Map<string, string array>,
            headers: Map<string, string>,
            response: HttpResponse
        ) =
        async {
            if not (query_params.ContainsKey "table") then
                response.MakeErrorResponse(400, "'table' is required") |> ignore
            else

            let table = query_params.["table"].[0]

            if table <> "crescent" then
                raise NotFoundException
            else

                let suggestions = TableAddSuggestion.list table

                response.ReplyJson(
                    {
                        Suggestions =
                            suggestions
                            |> Array.map (fun x ->
                                {
                                    ChartId = x.ChartId
                                    OsuBeatmapId = x.OsuBeatmapId
                                    EtternaPackId = x.EtternaPackId
                                    Artist = x.Artist
                                    Title = x.Title
                                    Difficulty = x.Difficulty
                                    SuggestedLevel = x.SuggestedLevel
                                }
                            )
                    }
                    : Tables.Suggestions.Response
                )
        }
