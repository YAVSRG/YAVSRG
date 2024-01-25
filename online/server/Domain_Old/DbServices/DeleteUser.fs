namespace Interlude.Web.Server.Domain.Old

open StackExchange.Redis
open Percyqaz.Common
open Interlude.Web.Server.Domain.Redis

module Aggregate =

    let delete_user (userId: int64) =
        match User.by_id userId with
        | Some user ->
            Logging.Info(
                sprintf "Deleting user with id %i, discord id %i, username '%s'" userId user.DiscordId user.Username
            )

            let server = redis.GetServers().[0]

            let leaderboard_keys =
                server.Keys(pattern = RedisValue("leaderboard:*:*")) |> Array.ofSeq

            for k in leaderboard_keys do
                db.SortedSetRemove(k, userId) |> ignore

            Logging.Info(sprintf "Removed %s from all leaderboards (%i)" user.Username leaderboard_keys.Length)

            // todo: remove from table rankings

            let score_keys =
                server.Keys(pattern = RedisValue(sprintf "scores:%i:*" userId)) |> Array.ofSeq

            for k in score_keys do
                json.Del(k) |> ignore

            Logging.Info(sprintf "Removed %s's scores (%i)" user.Username score_keys.Length)

            json.Del(Friends.key userId) |> ignore
            Logging.Info(sprintf "Removed %s's friend list" user.Username)

            json.Del(User.key userId) |> ignore
            Logging.Info(sprintf "Deleting %s complete" user.Username)

        | None -> failwithf "No such user with id %i" userId
