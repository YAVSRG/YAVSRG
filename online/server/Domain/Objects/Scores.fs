namespace Interlude.Web.Server.Domain

open System
open StackExchange.Redis
open NRedisStack.Search
open NRedisStack.Search.Aggregation
open Interlude.Web.Server.Domain.Redis

open Prelude.Gameplay

type Score =
    {
        UserId: int64
        ChartId: string
        RulesetId: string
        Score: float
        Grade: int
        Lamp: int
        Rate: float32
        Mods: Mods.ModState
        Timestamp: int64
    }

module Score =

    let SHORT_TERM_RULESET_LIST = [| "SC(J4)548E5A" |]

    let private key (id: int64) = RedisKey("score:" + id.ToString())

    let private save (id, score: Score) = json.Set(key id, "$", score) |> ignore

    let save_new (score: Score) : int64 =
        let new_id = db.StringIncrement("count:scores", 1L)
        save (new_id, score)
        new_id

    let get_recent (userId: int64) =
        let results =
            ft
                .Search(
                    "idx:scores",
                    Query(sprintf "@user_id:[%i %i]" userId userId)
                        .SetSortBy("timestamp", false)
                        .Limit(0, 10)
                )
                .Documents

        results
        |> Seq.map (fun d -> Text.Json.JsonSerializer.Deserialize<Score>(d.Item "json"))
        |> Array.ofSeq

    let exists (userId: int64) (timestamp: int64) =
        ft
            .Search(
                "idx:scores",
                Query(sprintf "@user_id:[%i %i] @timestamp:[%i %i]" userId userId timestamp timestamp)
            )
            .Documents
        |> Seq.isEmpty
        |> not

    let get_best_score (userId: int64) (ruleset_id: string) (rate: float32) (hash: string) : Score option =
        let results =
            ft
                .Search(
                    "idx:scores",
                    Query(
                        sprintf
                            "@user_id:[%i %i] @rate:[%f 2.0] @hash:{%s} @ruleset_id:{%s}"
                            userId
                            userId
                            rate
                            hash
                            (ruleset_id.Replace(")", "\\)").Replace("(", "\\("))
                    )
                        .SetSortBy("score", false)
                        .Limit(0, 1)
                )
                .Documents

        results
        |> Seq.tryHead
        |> Option.map (fun d -> Text.Json.JsonSerializer.Deserialize<Score>(d.Item "json"))

    // ft.aggregate idx:scores "@user_id:[4 4]" verbatim groupby 1 @hash reduce max 1 grade as best_grade
    // todo: filter to table hashes when there are more scores for non-table than table (soon)

    let aggregate_table_grades (userId: int64) (ruleset_id: string) (rate: float32) =
        let results = Collections.Generic.Dictionary<string, int>()

        ft
            .Aggregate(
                "idx:scores",
                AggregationRequest(
                    sprintf
                        "@user_id:[%i %i] @rate:[%f 2.0] @ruleset_id:{%s}"
                        userId
                        userId
                        rate
                        (ruleset_id.Replace(")", "\\)").Replace("(", "\\("))
                )
                    .Verbatim()
                    .GroupBy("@hash", Reducers.Max("grade").As("best_grade"))
            )
            .GetResults()
        |> Seq.iter (fun result ->
            let mutable g = 0
            result.["best_grade"].TryParse(&g) |> ignore
            results.[result.["hash"].ToString()] <- g
        )

        results

    let aggregate_table_scores (userId: int64) (ruleset_id: string) (rate: float32) =
        let results = Collections.Generic.Dictionary<string, float>()

        ft
            .Aggregate(
                "idx:scores",
                AggregationRequest(
                    sprintf
                        "@user_id:[%i %i] @rate:[%f 2.0] @ruleset_id:{%s}"
                        userId
                        userId
                        rate
                        (ruleset_id.Replace(")", "\\)").Replace("(", "\\("))
                )
                    .Verbatim()
                    .GroupBy("@hash", Reducers.Max("score").As("best_score"))
            )
            .GetResults()
        |> Seq.iter (fun result ->
            let mutable a = 0.0
            result.["best_score"].TryParse(&a) |> ignore
            results.[result.["hash"].ToString()] <- a
        )

        results