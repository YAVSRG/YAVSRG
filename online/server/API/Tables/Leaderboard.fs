namespace Interlude.Web.Server.API.Tables

open NetCoreServer
open Interlude.Web.Shared
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain.Services

module Leaderboard =

    open Tables.Leaderboard

    let handle
        (
            body: string,
            query_params: Map<string, string array>,
            headers: Map<string, string>,
            response: HttpResponse
        ) =
        async {
            require_query_parameter query_params "table"
            let _, _ = authorize headers

            let table_id = query_params.["table"].[0]

            if not (Backbeat.Tables.exists table_id) then
                raise NotFoundException
            else

            let info = Tables.get_leaderboard_details table_id

            let players: Player array =
                info
                |> Array.map (fun (i, user, rating) ->
                    {
                        Username = user.Username
                        Color = user.Color
                        Rank = i + 1
                        Rating = rating
                    }
                )

            response.ReplyJson({ Players = players }: Response)
        }