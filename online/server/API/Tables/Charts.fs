namespace Interlude.Web.Server.API.Tables

open NetCoreServer
open Interlude.Web.Shared
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain.Backbeat
open Interlude.Web.Server.Domain.Services

module Charts =

    open Tables.Charts

    let handle
        (
            body: string,
            query_params: Map<string, string array>,
            headers: Map<string, string>,
            response: HttpResponse
        ) =
        async {
            require_query_parameter query_params "table"

            let table_id = query_params.["table"].[0].ToLower()

            let section_name =
                match query_params.TryFind "section" with
                | Some ps -> Some(ps.[0].ToLower())
                | None -> None

            if not (Backbeat.Tables.exists table_id) then
                raise NotFoundException
            else

            let table = Backbeat.Tables.TABLES.[table_id]

            let charts =
                match section_name with
                | Some name ->
                    match table.Sections |> Seq.tryFind (fun x -> x.Name.ToLower() = name) with
                    | Some section -> TableLevel.get_range table_id section.LevelStart section.LevelEnd
                    | None -> raise NotFoundException
                | None -> TableLevel.get_all table_id

            let charts_with_info: ChartInfo array =
                charts
                |> Array.choose (fun (chart_id, level) ->
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

            response.ReplyJson({ Charts = charts_with_info }: Response)
        }