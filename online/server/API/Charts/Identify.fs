namespace Interlude.Web.Server.API.Charts

open NetCoreServer
open Interlude.Web.Shared
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain.Services

module Identify =

    open Charts.Identify

    let handle
        (
            body: string,
            query_params: Map<string, string array>,
            headers: Map<string, string>,
            response: HttpResponse
        ) =
        async {
            require_query_parameter query_params "chart"

            let hash = query_params.["chart"].[0].ToUpper()

            match Backbeat.Charts.by_hash hash with
            | Some(chart, song) ->
                response.ReplyJson(
                    {
                        Info = Some { Song = song; Chart = chart }
                    }
                    : Response
                )
            | None -> response.ReplyJson({ Info = None }: Response)
        }