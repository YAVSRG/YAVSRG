namespace Interlude.Web.Server.Domain

open System
open StackExchange.Redis
open NRedisStack.Search
open Interlude.Web.Server.Domain.Redis

module TableRanking =

    let key (id: string) = RedisKey(sprintf "tableranking:%s" id)

    let rank (id: string) (userId: int64) : int64 option =
        let result = db.SortedSetRank(key id, userId, Order.Descending)
        if result.HasValue then Some result.Value else None

    let rating (id: string) (userId: int64) : float option =
        let result = db.SortedSetScore(key id, userId)
        if result.HasValue then Some result.Value else None

    let update (id: string) (userId: int64) (rating: float) : int64 =
        db.SortedSetAdd(key id, userId, rating) |> ignore
        let result = db.SortedSetRank(key id, userId, Order.Descending)
        result.Value

    let get_top_50_ids (id: string) =
        db.SortedSetRangeByRankWithScores(key id, 0, 50, Order.Descending)
        |> Array.map (fun v ->
            let mutable r = 0L
            let _ = v.Element.TryParse(&r)
            r, v.Score
        )

    let get_top_50_info (id: string) =
        let userIds = get_top_50_ids id

        let users =
            userIds
            |> Array.map fst
            |> User.by_ids
            |> Array.map (Option.map (fun x -> x.Username, x.Color))

        Array.zip users (userIds |> Array.map snd)

type TableSuggestion =
    {
        UserId: int64
        ChartId: string
        OsuBeatmapId: int
        EtternaPackId: int
        Artist: string
        Title: string
        Difficulty: string
        TableFor: string
        SuggestedLevel: int
        Timestamp: int64
    }

module TableSuggestion =

    let id (key: string) = key.Substring(18) |> int64
    let key (id: int64) =
        RedisKey(sprintf "table_suggest_add:%i" id)

    let save (id, suggestion: TableSuggestion) =
        json.Set(key id, "$", suggestion) |> ignore

    let save_new (suggestion: TableSuggestion) : int64 =
        let new_id = db.StringIncrement("count:table_suggest_add", 1L)
        save (new_id, suggestion)
        new_id

    let try_get_existing (chart_id: string, table_for: string) =
        ft
            .Search(
                "idx:table_suggest_add",
                Query(sprintf "@chart_id:{%s} @table_for:{%s}" chart_id table_for)
                    .SetSortBy("timestamp", true)
            )
            .Documents
        |> Seq.tryExactlyOne
        |> Option.map (fun d -> id d.Id, Text.Json.JsonSerializer.Deserialize<TableSuggestion>(d.Item "json"))

    let list (table_for: string) =
        ft
            .Search(
                "idx:table_suggest_add",
                Query(sprintf "@table_for:{%s}" table_for)
                    .SetSortBy("timestamp", true)
                    .Limit(0, 100)
            )
            .Documents
        |> Seq.map (fun d -> id d.Id, Text.Json.JsonSerializer.Deserialize<TableSuggestion>(d.Item "json"))
        |> Array.ofSeq