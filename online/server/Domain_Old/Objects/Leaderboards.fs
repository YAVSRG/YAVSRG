namespace Interlude.Web.Server.Domain.Old

open System
open StackExchange.Redis
open Interlude.Web.Server.Domain.Redis

open Prelude.Gameplay

module Leaderboard =

    type Replay =
        {
            Replay: string
            Rate: float32
            Mods: Mods.ModState
            Timestamp: int64
            UploadTimestamp: int64
        }

    module Replay =

        let key (hash: string) (ruleset: string) (userId: int64) =
            RedisKey(sprintf "leaderboard.score:%s:%s:%i" hash ruleset userId)

        let create (replay: string, rate: float32, mods: Mods.ModState, timestamp: DateTime) =
            {
                Replay = replay
                Rate = MathF.Round(rate, 2)
                Mods = mods
                Timestamp = (DateTimeOffset.op_Implicit timestamp).ToUnixTimeMilliseconds()
                UploadTimestamp = DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()
            }

        let save (hash: string) (ruleset: string) (userId: int64) (score: Replay) =
            json.Set(key hash ruleset userId, "$", score) |> ignore

        let get (hash: string) (ruleset: string) (userId: int64) =
            let result = json.Get(key hash ruleset userId, [| "$" |])

            if result.IsNull then
                None
            else
                let s: string = RedisResult.op_Explicit result
                Some <| Text.Json.JsonSerializer.Deserialize<Replay>(s)

        let get_chart_scores (hash: string) (ruleset: string) (userIds: int64 array) =
            if userIds.Length = 0 then
                [||]
            else

                json.MGet(userIds |> Array.map (fun id -> key hash ruleset id), "$")
                |> Array.map (fun result ->
                    if result.IsNull then
                        None
                    else
                        let s: string = RedisResult.op_Explicit result
                        Some <| Text.Json.JsonSerializer.Deserialize<Replay array>(s).[0]
                )

    let private key (hash: string) (ruleset: string) =
        RedisKey(sprintf "leaderboard:%s:%s" hash ruleset)

    let private existence_key (hash: string) =
        RedisKey(sprintf "leaderboards:%s" hash)

    let add_score (hash: string) (ruleset: string) (userId: int64) (score: float) =
        db.SortedSetAdd(key hash ruleset, userId, score) |> ignore
        let result = db.SortedSetRank(key hash ruleset, userId, Order.Descending)
        result.Value

    let get_top_20_ids (hash: string) (ruleset: string) =
        db.SortedSetRangeByRankWithScores(key hash ruleset, 0, 20, Order.Descending)
        |> Array.map (fun v ->
            let mutable r = 0L
            let _ = v.Element.TryParse(&r)
            r, v.Score
        )

    let get_top_20_info (hash: string) (ruleset: string) =
        let userIds = get_top_20_ids hash ruleset |> Array.map fst

        Array.zip
            (User.by_ids userIds |> Array.map (Option.map (fun x -> x.Username)))
            (Replay.get_chart_scores hash ruleset userIds)

    let user_by_rank (hash: string) (ruleset: string) (rank: int64) =
        db.SortedSetRangeByRankWithScores(key hash ruleset, rank, rank + 1L, Order.Descending)
        |> Seq.tryExactlyOne
        |> Option.map (fun v ->
            let mutable r = 0L
            let _ = v.Element.TryParse(&r)
            r, v.Score
        )

    let rank (hash: string) (ruleset: string) (userId: int64) : int64 option =
        let result = db.SortedSetRank(key hash ruleset, userId, Order.Descending)
        if result.HasValue then Some result.Value else None

    let score (hash: string) (ruleset: string) (userId: int64) : float option =
        let result = db.SortedSetScore(key hash ruleset, userId)
        if result.HasValue then Some result.Value else None

    let exists (hash: string) (ruleset: string) =
        db.SetContains(existence_key hash, ruleset)

    let create (hash: string) (ruleset: string) =
        db.SetAdd(existence_key (hash.ToUpper()), ruleset) |> ignore

    let rulesets_by_hash (hash: string) =
        db.SetScan(existence_key hash) |> Seq.map (fun v -> v.ToString()) |> Array.ofSeq
