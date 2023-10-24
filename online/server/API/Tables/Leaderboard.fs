namespace Interlude.Web.Server.API.Tables

open NetCoreServer
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain

module Leaderboard =

    let handle (body: string, query_params: Map<string, string array>, headers: Map<string, string>, response: HttpResponse) = 
        async {
            if not (query_params.ContainsKey "table") then
                response.MakeErrorResponse(400, "'table' is required") |> ignore
            else

            let _, _ = authorize headers

            let table_id = query_params.["table"].[0]
            
            if table_id <> "crescent" then raise NotFoundException else
            
            let info = TableRanking.get_top_50_info "crescent"

            let players : Tables.Leaderboard.Player array =
                info 
                |> Array.indexed 
                |> Array.choose (
                    function
                    | i, (Some (username, color), rating) -> 
                        Some { 
                            Username = username
                            Color = color |> Option.defaultValue Badge.DEFAULT_COLOR
                            Rank = i + 1
                            Rating = rating
                        }
                    | _ -> None
                    )

            response.ReplyJson({ Players = players } : Tables.Leaderboard.Response)
        }