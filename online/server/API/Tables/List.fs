namespace Interlude.Web.Server.API.Tables

open NetCoreServer
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain.Services

module List =

    open Tables.List

    let handle
        (
            body: string,
            query_params: Map<string, string array>,
            headers: Map<string, string>,
            response: HttpResponse
        ) =
        async {
            let tables : Table array = 
                Backbeat.Tables.TABLES
                |> Map.toArray
                |> Array.map (fun (id, info) -> { Id = id; Info = info })

            response.ReplyJson({ Tables = tables } : Response)
        }
