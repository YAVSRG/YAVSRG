namespace Interlude.Web.Server.API.Stats.Leaderboard

open NetCoreServer
open Percyqaz.Common
open Prelude.Data.User.Stats
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain.Core

module MonthlyXP =

    open Stats.Leaderboard.MonthlyXP

    let handle
        (
            body: string,
            query_params: Map<string, string array>,
            headers: Map<string, string>,
            response: HttpResponse
        ) =
        async {
            let _, _ = authorize headers

            let by_playtime = query_params.TryFind "sort" |> Option.bind Array.tryHead = Some "playtime"

            let month = Timestamp.now() |> timestamp_to_leaderboard_month

            let data =
                if by_playtime then
                    MonthlyStats.playtime_leaderboard month
                else
                    MonthlyStats.xp_leaderboard month

            let users =
                data |> Array.map (fun x -> x.UserId) |> User.by_ids |> Map.ofArray

            let result =
                data
                |> Array.map (fun l ->
                    let user = users.[l.UserId]
                    {
                        Username = user.Username
                        Color = user.Color
                        XP = l.XP
                        Playtime = l.Playtime
                    }
                )

            response.ReplyJson(result: Response)
        }