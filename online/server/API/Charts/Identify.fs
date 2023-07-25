namespace Interlude.Web.Server.API.Charts

open NetCoreServer
open System.Net.Http
open Percyqaz.Json
open Interlude.Web.Shared.API
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain

module Identify =

    let ROUTE = (GET, "/charts/identify")

    [<Json.AutoCodec>]
    type Response =
        {
            Found: bool
            Song: Prelude.Backbeat.Archive.Song
            Mirrors: string list
        }

    let handle (body: string, query_params: Map<string, string array>, headers: Map<string, string>, response: HttpResponse) = 
        async {
            if not (query_params.ContainsKey "id") then
                response.MakeErrorResponse(400) |> ignore
            else

            let hash = query_params.["id"].[0].ToUpper()
            match Charts.by_hash hash with
            | Some (chart, song) ->
                response.ReplyJson({ Found = true; Song = song; Mirrors = chart.Sources |> Charts.mirrors |> List.ofSeq })
            | None ->
                response.ReplyJson({| Found = false |})
        }