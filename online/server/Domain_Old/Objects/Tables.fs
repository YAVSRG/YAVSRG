namespace Interlude.Web.Server.Domain.Old

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

    //let get_top_50_info (id: string) =
    //    let userIds = get_top_50_ids id

    //    let users =
    //        userIds
    //        |> Array.map fst
    //        |> User.by_ids
    //        |> Array.map (Option.map (fun x -> x.Username, x.Color))

    //    Array.zip users (userIds |> Array.map snd)

type TableSuggestion =
    {
        ChartId: string
        TableFor: string
        OsuBeatmapId: int
        EtternaPackId: int
        Artist: string
        Title: string
        Creator: string
        Difficulty: string
        SuggestedLevels: Map<int64, int>
        LastUpdated: int64
    }

module TableSuggestion =

    let id (key: string) = key.Substring(17) |> int64

    let key (id: int64) =
        RedisKey(sprintf "table_suggestion:%i" id)

    let save (id, suggestion: TableSuggestion) =
        json.Set(key id, "$", suggestion) |> ignore

    let save_new (suggestion: TableSuggestion) : int64 =
        let new_id = db.StringIncrement("count:table_suggestions", 1L)
        save (new_id, suggestion)
        new_id

    let delete (id: int64) = json.Del(key id, "$") |> ignore

    let by_id (id: int64) =
        let result = json.Get(key id, [| "$" |])

        if result.IsNull then
            None
        else
            let s: string = RedisResult.op_Explicit result
            Some <| Text.Json.JsonSerializer.Deserialize<TableSuggestion array>(s).[0]

    let try_get_existing (chart_id: string, table_for: string) =
        ft
            .Search(
                "idx:table_suggestions",
                Query(sprintf "@chart_id:{%s} @table_for:{%s}" (escape chart_id) (escape table_for))
                    .SetSortBy("last_updated", true)
            )
            .Documents
        |> Seq.tryExactlyOne
        |> Option.map (fun d -> id d.Id, Text.Json.JsonSerializer.Deserialize<TableSuggestion>(d.Item "json"))

    let list (table_for: string) =
        ft
            .Search(
                "idx:table_suggestions",
                Query(sprintf "@table_for:{%s}" (escape table_for))
                    .SetSortBy("last_updated", true)
                    .Limit(0, 100)
            )
            .Documents
        |> Seq.map (fun d -> id d.Id, Text.Json.JsonSerializer.Deserialize<TableSuggestion>(d.Item "json"))
        |> Array.ofSeq

open Prelude.Common
open Prelude.Data.Charts.Tables

module TableWithSuggestions =

    let key (id: string) = RedisKey(sprintf "table_preview:%s" id)

    let get (id) =
        let result = db.StringGet(key id)

        if result.IsNullOrEmpty then
            None
        else
            let s: string = result.ToString()
            Result.toOption <| JSON.FromString<Table>(s)

    let update (id: string, table: Table) =
        db.StringSet(key id, JSON.ToString table) |> ignore

    let update_if_newer (id: string, table: Table) =
        if
            match get id with
            | None -> true
            | Some existing -> existing.Version < table.Version
        then
            update (id, table)
