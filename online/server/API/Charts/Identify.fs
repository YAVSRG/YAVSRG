namespace Interlude.Web.Server.API.Charts

open NetCoreServer
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain

module Identify =

    let handle
        (
            body: string,
            query_params: Map<string, string array>,
            headers: Map<string, string>,
            response: HttpResponse
        ) =
        async {
            if not (query_params.ContainsKey "chart") then
                response.MakeErrorResponse(400, "'chart' is required") |> ignore
            else

            let hash = query_params.["chart"].[0].ToUpper()

            match Backbeat.Charts.by_hash hash with
            | Some(chart, song) ->
                response.ReplyJson(
                    {
                        Info =
                            Some
                                {
                                    Song = song
                                    Chart = chart
                                    Mirrors = chart.Sources |> Backbeat.Charts.mirrors |> List.ofSeq
                                }
                    }
                    : Charts.Identify.Response
                )
            | None -> response.ReplyJson({ Info = None }: Charts.Identify.Response)
        }
