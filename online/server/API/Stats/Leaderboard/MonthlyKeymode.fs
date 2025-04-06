namespace Interlude.Web.Server.API.Stats.Leaderboard

open NetCoreServer
open Percyqaz.Common
open Prelude.Data.User.Stats
open Interlude.Web.Shared
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain.Core

module MonthlyKeymode =

    open Stats.Leaderboard
    open Stats.Leaderboard.MonthlyKeymode

    let handle
        (
            body: string,
            query_params: Map<string, string array>,
            headers: Map<string, string>,
            response: HttpResponse
        ) =
        async {
            let user_id, user = authorize headers

            let sort =
                match query_params.TryFind "sort" |> Option.bind Array.tryHead |> Option.map (_.ToLowerInvariant()) with
                | Some "playtime" -> Playtime
                | Some "combined" -> Combined
                | Some "jacks" -> Jacks
                | Some "chordstream" -> Chordstream
                | Some "stream" -> Stream
                | _ -> Combined

            let keys =
                match query_params.TryFind "keys" |> Option.bind Array.tryHead with
                | Some k ->
                    match System.Int32.TryParse(k) with
                    | true, keys -> keys
                    | false, _ -> 4
                | None -> 4

            let month = Timestamp.now() |> timestamp_to_leaderboard_month

            let data =
                match sort, keys with
                | Playtime, 4 -> MonthlyStats.leaderboard_4k_playtime month
                | Combined, 4 -> MonthlyStats.leaderboard_4k_combined month
                | Jacks, 4 -> MonthlyStats.leaderboard_4k_jacks month
                | Chordstream, 4 -> MonthlyStats.leaderboard_4k_chordstream month
                | Stream, 4 -> MonthlyStats.leaderboard_4k_stream month

                | Playtime, 7 -> MonthlyStats.leaderboard_7k_playtime month
                | Combined, 7 -> MonthlyStats.leaderboard_7k_combined month
                | Jacks, 7 -> MonthlyStats.leaderboard_7k_jacks month
                | Chordstream, 7 -> MonthlyStats.leaderboard_7k_chordstream month
                | Stream, 7 -> MonthlyStats.leaderboard_7k_stream month

                | _ -> [||]

            let users =
                data |> Array.map (fun x -> x.UserId) |> User.by_ids |> Map.ofArray

            let mutable you : (int64 * KeymodeLeaderboardEntry) option = None

            let mutable i = 0
            let result =
                data
                |> Array.map (fun l ->
                    i <- i + 1
                    let user = users.[l.UserId]
                    let entry =
                        {
                            Username = user.Username
                            Color = user.Color
                            Playtime = l.Playtime
                            Combined = l.Combined
                            Jacks = l.Jacks
                            Chordstream = l.Chordstream
                            Stream = l.Stream
                        }
                    if user_id = l.UserId then you <- Some (i, entry)
                    entry
                )

            match you with
            | Some _ -> ()
            | None ->
                match
                    match sort, keys with
                    | Playtime, 4 -> MonthlyStats.rank_4k_playtime user_id month
                    | Combined, 4 -> MonthlyStats.rank_4k_combined user_id month
                    | Jacks, 4 -> MonthlyStats.rank_4k_jacks user_id month
                    | Chordstream, 4 -> MonthlyStats.rank_4k_chordstream user_id month
                    | Stream, 4 -> MonthlyStats.rank_4k_stream user_id month

                    | Playtime, 7 -> MonthlyStats.rank_7k_playtime user_id month
                    | Combined, 7 -> MonthlyStats.rank_7k_combined user_id month
                    | Jacks, 7 -> MonthlyStats.rank_7k_jacks user_id month
                    | Chordstream, 7 -> MonthlyStats.rank_7k_chordstream user_id month
                    | Stream, 7 -> MonthlyStats.rank_7k_stream user_id month

                    | _ -> None
                with
                | Some rank ->
                    you <- Some (
                        rank.Rank,
                        {
                            Username = user.Username
                            Color = user.Color
                            Playtime = rank.Playtime
                            Combined = rank.Combined
                            Jacks = rank.Jacks
                            Chordstream = rank.Chordstream
                            Stream = rank.Stream
                        }
                    )
                | None -> ()

            response.ReplyJson({ Leaderboard = result; You = you }: Response)
        }