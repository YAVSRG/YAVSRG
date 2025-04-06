namespace Interlude.Web.Server.API.Tables

open NetCoreServer
open Interlude.Web.Shared
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain.Backbeat
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
            let tables: Table array =
                Backbeat.Tables.TABLES
                |> Map.toArray
                |> Array.map (fun (id, info) ->
                    {
                        Id = id
                        Info = info
                        LastUpdated = TableLevel.get_time_last_changed id |> Option.defaultValue 0L
                    }
                )

            response.ReplyJson({ Tables = tables }: Response)
        }