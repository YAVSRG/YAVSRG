namespace Interlude.Web.Server.Domain

open StackExchange.Redis
open Interlude.Web.Server.Domain.Redis

module Friends =

    let key (id: int64) = RedisKey("friends:" + id.ToString())

    let get_id_list (userId: int64) : int64 array =
        db.SetScan(key userId)
        |> Seq.map (fun v ->
            let mutable r = 0L
            let _ = v.TryParse(&r)
            r
        )
        |> Array.ofSeq

    let add_friend (userId: int64, friendId: int64) =
        if userId <> friendId then
            db.SetAdd(key userId, friendId) |> ignore

    let remove_friend (userId: int64, friendId: int64) =
        db.SetRemove(key userId, friendId) |> ignore

    let has_friend (userId: int64, friendId: int64) = db.SetContains(key userId, friendId)

    let friends_list (userId: int64) = User.by_ids (get_id_list userId)
