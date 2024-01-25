namespace Interlude.Web.Server.API.Tables.Suggestions

open NetCoreServer
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain
open Interlude.Web.Server.Domain.Old

module Missing =

    let handle
        (
            body: string,
            query_params: Map<string, string array>,
            headers: Map<string, string>,
            response: HttpResponse
        ) =
        async {
            require_query_parameter query_params "table"

            let table = query_params.["table"].[0]

            match Backbeat.tables.TryFind table with
            | None -> raise NotFoundException
            | _ ->

            let suggestions = TableSuggestion.list table

            response.ReplyJson(
                {
                    Suggestions =
                        suggestions
                        |> Array.filter (fun (_, x) -> (Backbeat.Charts.by_hash x.ChartId).IsNone)
                        |> Array.map (fun (id, x) ->
                            {
                                Id = id
                                ChartId = x.ChartId
                                OsuBeatmapId = x.OsuBeatmapId
                                EtternaPackId = x.EtternaPackId
                                Artist = x.Artist
                                Title = x.Title
                                Creator = x.Creator
                                Difficulty = x.Difficulty
                            }
                        )
                }
                : Tables.Suggestions.Missing.Response
            )
        }
