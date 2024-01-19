namespace Interlude.Web.Server.Domain.New

open System
open Percyqaz.Common
open Percyqaz.Data.Sqlite
open Prelude.Common
open Prelude.Gameplay
open Interlude.Web.Server

type Replay =
    {
        UserId: int64
        ChartId: string
        TimePlayed: int64
        TimeUploaded: int64
        Rate: float32
        Mods: Mods.ModState
        Data: byte array
    }

module Replay =

    let internal CREATE_TABLE : NonQuery<unit> =
        { NonQuery.without_parameters() with
            SQL = """
            CREATE TABLE replays (
                Id INTEGER PRIMARY KEY NOT NULL
                UserId INTEGER NOT NULL,
                ChartId TEXT NOT NULL,
                Purposes TEXT NOT NULL,
                TimePlayed INTEGER NOT NULL,
                TimeUploaded INTEGER NOT NULL,
                Rate REAL NOT NULL,
                Mods TEXT NOT NULL,
                Data BLOB NOT NULL,
                FOREIGN KEY (UserId) REFERENCES users(Id) ON DELETE CASCADE
                UNIQUE (UserId, ChartId, TimePlayed)
            );
            """
        }

    let create (user_id: int64, chart_id: string, timestamp: DateTime, replay: ReplayData, rate: float32, mods: Mods.ModState) =
        {
            UserId = user_id
            ChartId = chart_id
            TimePlayed = (DateTimeOffset.op_Implicit timestamp).ToUnixTimeMilliseconds()
            TimeUploaded = DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()
            Rate = MathF.Round(rate, 2)
            Mods = mods
            Data = Replay.compress_bytes replay
        }

    let private SAVE_LEADERBOARD : NonQuery<string * Replay> =
        {
            SQL = """
            BEGIN TRANSACTION;

            UPDATE replays
            SET Purposes = JSON_GROUP_ARRAY(json_each.value)
            FROM (
                SELECT replays.UserId, replays.ChartId, replays.Purposes, json_each.value
                FROM replays, json_each(replays.Purposes)
                WHERE UserId = @UserId
                AND ChartId = @ChartId
                AND json_each.value IS NOT @Purpose
            )

            DELETE FROM replays
            WHERE UserId = @UserId
            AND ChartId = @ChartId
            AND Purpose = '[]';

            INSERT INTO replays (UserId, ChartId, Purposes, TimePlayed, TimeUploaded, Rate, Mods, Data)
            VALUES (@UserId, @ChartId, @PurposeSingleton, @TimePlayed, @TimeUploaded, @Rate, @Mods, @Data)
            ON CONFLICT DO UPDATE SET 
                TimeUploaded = excluded.TimeUploaded
                Purposes = JSON_GROUP_ARRAY(DISTINCT JSON_EXTRACT(excluded.Purposes, '$') || @PurposeSingleton);

            COMMIT;
            """
            Parameters =
                [
                    "@UserId", SqliteType.Integer, 8
                    "@ChartId", SqliteType.Text, -1
                    "@TimePlayed", SqliteType.Integer, 8
                    "@TimeUploaded", SqliteType.Integer, 8
                    "@Rate", SqliteType.Real, 4
                    "@Mods", SqliteType.Text, -1
                    "@Data", SqliteType.Blob, -1
                    "@PurposeSingleton", SqliteType.Text, -1
                    "@Purpose", SqliteType.Text, -1
                ]
            FillParameters = (fun p (ruleset, replay) ->
                p.Int64 replay.UserId
                p.String replay.ChartId
                p.Int64 replay.TimePlayed
                p.Int64 replay.TimeUploaded
                p.Float32 replay.Rate
                p.Json JSON replay.Mods
                p.Blob replay.Data
                p.Json JSON [ ruleset ]
                p.String ruleset
            )
        }
    // Only the most best replay per-chart is stored, overwriting any previous replay for that chart
    let save_leaderboard (ruleset_id: string) (replay: Replay) = SAVE_LEADERBOARD.ExecuteGetId (ruleset_id, replay) db |> expect |> ignore

    let private SAVE_CHALLENGE : NonQuery<Replay> =
        {
            SQL = """
            INSERT INTO replays (UserId, ChartId, Purposes, TimePlayed, TimeUploaded, Rate, Mods, Data)
            VALUES (@UserId, @ChartId, @PurposeSingleton, @TimePlayed, @TimeUploaded, @Rate, @Mods, @Data)
            ON CONFLICT DO UPDATE SET 
                TimeUploaded = excluded.TimeUploaded
                Purposes = JSON_GROUP_ARRAY(DISTINCT JSON_EXTRACT(excluded.Purposes, '$') || @PurposeSingleton);
            """
            Parameters =
                [
                    "@UserId", SqliteType.Integer, 8
                    "@ChartId", SqliteType.Text, -1
                    "@TimePlayed", SqliteType.Integer, 8
                    "@TimeUploaded", SqliteType.Integer, 8
                    "@Rate", SqliteType.Real, 4
                    "@Mods", SqliteType.Text, -1
                    "@Data", SqliteType.Blob, -1
                    "@PurposeSingleton", SqliteType.Text, -1
                ]
            FillParameters = (fun p replay ->
                p.Int64 replay.UserId
                p.String replay.ChartId
                p.Int64 replay.TimePlayed
                p.Int64 replay.TimeUploaded
                p.Float32 replay.Rate
                p.Json JSON replay.Mods
                p.Blob replay.Data
                p.Json JSON ["challenge"]
            )
        }
    // Replay is stored long term for sharing with friends
    let save_challenge (replay: Replay) = SAVE_CHALLENGE.ExecuteGetId replay db |> expect

    let private BY_ID : Query<int64, Replay> =
        {
            SQL = """
            SELECT Id, UserId, ChartId, TimePlayed, TimeUploaded, Rate, Mods FROM replays
            WHERE Id = @Id;
            """
            Parameters = [ "@Id", SqliteType.Integer, 8 ]
            FillParameters = fun p id -> p.Int64 id
            Read = (fun r ->
                r.Int64 |> ignore;
                {
                    UserId = r.Int64
                    ChartId = r.String
                    TimePlayed = r.Int64
                    TimeUploaded = r.Int64
                    Rate = r.Float32
                    Mods = r.Json JSON
                    Data = [||]
                }
            )
        }
    let by_id (replay_id: int64) = BY_ID.Execute replay_id db |> expect |> Array.tryExactlyOne

//type Score =
//    {
//        UserId: int64
//        ChartId: string
//        RulesetId: string
//        Score: float
//        Grade: int
//        Lamp: int
//        Rate: float32
//        Mods: Mods.ModState
//        Timestamp: int64
//    }

//module Score =

//    let RULESETS = [| "SC(J4)548E5A" |]

//    let private key (id: int64) = RedisKey("score:" + id.ToString())

//    let private save (id, score: Score) = json.Set(key id, "$", score) |> ignore

//    let save_new (score: Score) : int64 =
//        let new_id = db.StringIncrement("count:scores", 1L)
//        save (new_id, score)
//        new_id

//    let get_recent (userId: int64) =
//        let results =
//            ft
//                .Search(
//                    "idx:scores",
//                    Query(sprintf "@user_id:[%i %i]" userId userId)
//                        .SetSortBy("timestamp", false)
//                        .Limit(0, 10)
//                )
//                .Documents

//        results
//        |> Seq.map (fun d -> Text.Json.JsonSerializer.Deserialize<Score>(d.Item "json"))
//        |> Array.ofSeq

//    let exists (userId: int64) (timestamp: int64) =
//        ft
//            .Search(
//                "idx:scores",
//                Query(sprintf "@user_id:[%i %i] @timestamp:[%i %i]" userId userId timestamp timestamp)
//            )
//            .Documents
//        |> Seq.isEmpty
//        |> not

//    let get_best_score (userId: int64) (ruleset_id: string) (rate: float32) (hash: string) : Score option =
//        let results =
//            ft
//                .Search(
//                    "idx:scores",
//                    Query(
//                        sprintf
//                            "@user_id:[%i %i] @rate:[%f 2.0] @hash:{%s} @ruleset_id:{%s}"
//                            userId
//                            userId
//                            rate
//                            (escape hash)
//                            (escape <| ruleset_id.Replace(")", "\\)").Replace("(", "\\("))
//                    )
//                        .SetSortBy("score", false)
//                        .Limit(0, 1)
//                )
//                .Documents

//        results
//        |> Seq.tryHead
//        |> Option.map (fun d -> Text.Json.JsonSerializer.Deserialize<Score>(d.Item "json"))

//    // ft.aggregate idx:scores "@user_id:[4 4]" verbatim groupby 1 @hash reduce max 1 grade as best_grade
//    // todo: filter to table hashes when there are more scores for non-table than table (soon)

//    let aggregate_table_grades (userId: int64) (ruleset_id: string) (rate: float32) =
//        let results = Collections.Generic.Dictionary<string, int>()

//        ft
//            .Aggregate(
//                "idx:scores",
//                AggregationRequest(
//                    sprintf
//                        "@user_id:[%i %i] @rate:[%f 2.0] @ruleset_id:{%s}"
//                        userId
//                        userId
//                        rate
//                        (escape <| ruleset_id.Replace(")", "\\)").Replace("(", "\\("))
//                )
//                    .Verbatim()
//                    .GroupBy("@hash", Reducers.Max("grade").As("best_grade"))
//            )
//            .GetResults()
//        |> Seq.iter (fun result ->
//            let mutable g = 0
//            result.["best_grade"].TryParse(&g) |> ignore
//            results.[result.["hash"].ToString()] <- g
//        )

//        results

//    let aggregate_table_scores (userId: int64) (ruleset_id: string) (rate: float32) =
//        let results = Collections.Generic.Dictionary<string, float>()

//        ft
//            .Aggregate(
//                "idx:scores",
//                AggregationRequest(
//                    sprintf
//                        "@user_id:[%i %i] @rate:[%f 2.0] @ruleset_id:{%s}"
//                        userId
//                        userId
//                        rate
//                        (escape <| ruleset_id.Replace(")", "\\)").Replace("(", "\\("))
//                )
//                    .Verbatim()
//                    .GroupBy("@hash", Reducers.Max("score").As("best_score"))
//            )
//            .GetResults()
//        |> Seq.iter (fun result ->
//            let mutable a = 0.0
//            result.["best_score"].TryParse(&a) |> ignore
//            results.[result.["hash"].ToString()] <- a
//        )

//        results
