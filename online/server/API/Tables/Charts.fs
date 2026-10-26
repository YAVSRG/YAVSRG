namespace Interlude.Web.Server.API.Tables

open NetCoreServer
open Interlude.Web.Shared
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain.Backbeat
open Interlude.Web.Server.Domain.Services

module Charts =

    open Tables.Charts

    let mutable cached_responses: Map<string, Response> = Map.empty
    let LOCK_OBJ = obj()

    let handle
        (body: string, query_params: Map<string, string array>, headers: Map<string, string>, response: HttpResponse)
        =
        async {
            require_query_parameter query_params "table"

            let table_id = query_params.["table"].[0].ToLower()

            if not(Backbeat.Tables.exists(table_id)) then
                raise NotFoundException
            else

            let generate_response (table_id: string) : Response =
                let table = Backbeat.Tables.TABLES.[table_id]
                let charts = TableLevel.get_all(table_id)

                let charts_with_info: ChartInfo array =
                    charts
                    |> Array.choose(fun (chart_id, level) ->
                        match Backbeat.Charts.by_hash chart_id with
                        | Some(chart, song) ->
                            Some
                                {
                                    Hash = chart_id
                                    Level = level
                                    Song = song
                                    Chart = chart
                                }
                        | None -> None
                    )

                { Charts = charts_with_info }

            let data =
                lock
                    LOCK_OBJ
                    (fun () ->
                        match cached_responses.TryFind(table_id) with
                        | Some response -> response
                        | None ->
                            let response = generate_response(table_id)
                            cached_responses <- cached_responses.Add(table_id, response)
                            response
                    )

            response.ReplyJson(data)
        }
