namespace Interlude.Web.Server.Domain.Core

open System
open Percyqaz.Common
open Percyqaz.Data.Sqlite
open Prelude
open Prelude.Mods
open Interlude.Web.Server

type Score =
    {
        UserId: int64
        ChartId: string
        TimePlayed: int64
        TimeUploaded: int64
        Rate: Rate
        Mods: ModState
        Ranked: bool // precomputed; true only if mod state is valid and rate >= 1
        Accuracy: float
        Grade: int
        Lamp: int
        ReplayId: int64 option
    }
    member this.WithReplay(replay_id) = { this with ReplayId = Some replay_id }

module Score =

    let internal CREATE_TABLE_OLD: NonQuery<unit> =
        { NonQuery.without_parameters () with
            SQL =
                """
            CREATE TABLE scores (
                Id INTEGER PRIMARY KEY NOT NULL,
                UserId INTEGER NOT NULL,
                ChartId TEXT NOT NULL,
                RulesetId TEXT NOT NULL,
                TimePlayed INTEGER NOT NULL,
                TimeUploaded INTEGER NOT NULL,
                Rate REAL NOT NULL,
                Mods TEXT NOT NULL,
                Ranked INTEGER NOT NULL,
                Accuracy REAL NOT NULL,
                Grade INTEGER NOT NULL,
                Lamp INTEGER NOT NULL,
                ReplayId INTEGER,
                FOREIGN KEY (UserId) REFERENCES users(Id) ON DELETE CASCADE,
                FOREIGN KEY (ReplayId) REFERENCES replays(Id) ON DELETE SET NULL,
                UNIQUE (UserId, ChartId, RulesetId, TimePlayed)
            );
            """
        }

    let internal MIGRATE_OLD_TO_NEW: NonQuery<unit> =
        { NonQuery.without_parameters () with
            SQL =
                """
            INSERT INTO replays2 (Id, UserId, ChartId, Persistent, TimePlayed, TimeUploaded, Data)
            SELECT Id, UserId, ChartId, 0, TimePlayed, TimeUploaded, Data FROM replays;

            INSERT INTO scores2 (Id, UserId, ChartId, TimePlayed, TimeUploaded, Rate, Mods, Ranked, Accuracy, Grade, Lamp, ReplayId)
            SELECT Id, UserId, ChartId, TimePlayed, TimeUploaded, Rate, Mods, Ranked, Accuracy, Grade, Lamp, ReplayId FROM scores;
            """
        }

    let internal CREATE_TABLE: NonQuery<unit> =
        { NonQuery.without_parameters () with
            SQL =
                """
            CREATE TABLE scores2 (
                Id INTEGER PRIMARY KEY NOT NULL,
                UserId INTEGER NOT NULL,
                ChartId TEXT NOT NULL,
                TimePlayed INTEGER NOT NULL,
                TimeUploaded INTEGER NOT NULL,
                Rate REAL NOT NULL,
                Mods TEXT NOT NULL,
                Ranked INTEGER NOT NULL,
                Accuracy REAL NOT NULL,
                Grade INTEGER NOT NULL,
                Lamp INTEGER NOT NULL,
                ReplayId INTEGER,
                FOREIGN KEY (UserId) REFERENCES users(Id) ON DELETE CASCADE,
                FOREIGN KEY (ReplayId) REFERENCES replays2(Id) ON DELETE SET NULL,
                UNIQUE (UserId, ChartId, TimePlayed)
            );
            """
        }

    let create
        (
            user_id: int64,
            chart_id: string,
            time_played: int64,
            rate: Rate,
            mods: ModState,
            is_ranked: bool,
            accuracy: float,
            grade: int,
            lamp: int
        ) =
        {
            UserId = user_id
            ChartId = chart_id
            TimePlayed = time_played
            TimeUploaded = Timestamp.now ()
            Rate = MathF.Round(float32 rate, 2) * 1.0f<rate>
            Mods = mods
            Ranked = is_ranked
            Accuracy = accuracy
            Grade = grade
            Lamp = lamp
            ReplayId = None
        }

    let private SAVE: Query<Score, int64> =
        {
            SQL =
                """
            INSERT INTO scores2 (UserId, ChartId, TimePlayed, TimeUploaded, Rate, Mods, Ranked, Accuracy, Grade, Lamp, ReplayId)
            VALUES (@UserId, @ChartId, @TimePlayed, @TimeUploaded, @Rate, @Mods, @Ranked, @Accuracy, @Grade, @Lamp, @ReplayId)
            ON CONFLICT DO UPDATE SET
                TimeUploaded = excluded.TimeUploaded,
                Ranked = excluded.Ranked,
                ReplayId = excluded.ReplayId
            RETURNING Id;
            """
            Parameters =
                [
                    "@UserId", SqliteType.Integer, 8
                    "@ChartId", SqliteType.Text, -1
                    "@TimePlayed", SqliteType.Integer, 8
                    "@TimeUploaded", SqliteType.Integer, 8
                    "@Rate", SqliteType.Real, 4
                    "@Mods", SqliteType.Text, -1
                    "@Ranked", SqliteType.Integer, 1
                    "@Accuracy", SqliteType.Real, 8
                    "@Grade", SqliteType.Integer, 4
                    "@Lamp", SqliteType.Integer, 4
                    "@ReplayId", SqliteType.Integer, 8
                ]
            FillParameters =
                (fun p score ->
                    p.Int64 score.UserId
                    p.String score.ChartId
                    p.Int64 score.TimePlayed
                    p.Int64 score.TimeUploaded
                    p.Float32 (float32 score.Rate)
                    p.Json JSON score.Mods
                    p.Boolean score.Ranked
                    p.Float64 score.Accuracy
                    p.Int32 score.Grade
                    p.Int32 score.Lamp
                    p.Int64Option score.ReplayId
                )
            Read = fun r -> r.Int64
        }

    let save (score: Score) : int64 =
        SAVE.Execute score core_db |> expect |> Array.exactlyOne

    let PRIMARY_RULESET = "SAE1C74D1"

    type RecentScore =
        {
            Id: int64
            ChartId: string
            TimePlayed: int64
            Rate: Rate
            Mods: ModState
            Accuracy: float
            Grade: int
            Lamp: int
        }

    let private GET_USER_RECENT: Query<int64, RecentScore> =
        {
            SQL =
                """
            SELECT Id, ChartId, TimePlayed, Rate, Mods, Accuracy, Grade, Lamp FROM scores2
            WHERE UserId = @UserId
            ORDER BY TimePlayed DESC
            LIMIT 10;
            """
            Parameters = [ "@UserId", SqliteType.Integer, 8 ]
            FillParameters = fun p id -> p.Int64 id
            Read =
                (fun r ->
                    {
                        Id = r.Int64
                        ChartId = r.String
                        TimePlayed = r.Int64
                        Rate = r.Float32 * 1.0f<rate>
                        Mods = r.Json JSON
                        Accuracy = r.Float64
                        Grade = r.Int32
                        Lamp = r.Int32
                    }
                )
        }

    let get_user_recent (user_id: int64) =
        GET_USER_RECENT.Execute user_id core_db |> expect

    type LeaderboardScore =
        {
            UserId: int64
            TimePlayed: int64
            Rate: Rate
            Mods: ModState
            Accuracy: float
            Grade: int
            Lamp: int
            ReplayId: int64 option
        }

    let LEADERBOARD_SIZE = 20
    let private GET_LEADERBOARD: Query<string, LeaderboardScore> =
        {
            SQL =
                """
            WITH UserBestScores AS (
                SELECT
                    UserId, TimePlayed, Rate, Mods, Accuracy, Grade, Lamp, ReplayId,
                    ROW_NUMBER() OVER (PARTITION BY UserId ORDER BY Accuracy DESC, TimePlayed ASC) AS UserScoreRank
                FROM scores2
                WHERE ChartId = @ChartId AND Ranked = 1
            )

            SELECT UserId, TimePlayed, Rate, Mods, Accuracy, Grade, Lamp, ReplayId FROM UserBestScores
            WHERE UserScoreRank = 1
            ORDER BY Accuracy DESC, TimePlayed ASC
            LIMIT 20;
            """
            Parameters = [ "@ChartId", SqliteType.Text, -1 ]
            FillParameters =
                fun p chart_id ->
                    p.String chart_id
            Read =
                (fun r ->
                    {
                        UserId = r.Int64
                        TimePlayed = r.Int64
                        Rate = r.Float32 * 1.0f<rate>
                        Mods = r.Json JSON
                        Accuracy = r.Float64
                        Grade = r.Int32
                        Lamp = r.Int32
                        ReplayId = r.Int64Option
                    }
                )
        }

    let get_leaderboard (chart_id: string) =
        GET_LEADERBOARD.Execute chart_id core_db |> expect

    type UserLeaderboardScore =
        {
            TimePlayed: int64
            Rate: Rate
            Mods: ModState
            Accuracy: float
            Grade: int
            Lamp: int
            ReplayId: int64 option
        }

    let private GET_USER_LEADERBOARD_SCORE: Query<int64 * string, UserLeaderboardScore> =
        {
            SQL =
                """
            SELECT TimePlayed, Rate, Mods, Accuracy, Grade, Lamp, ReplayId FROM scores2
            WHERE UserId = @UserId AND Ranked = 1 AND ChartId = @ChartId
            ORDER BY Accuracy DESC
            LIMIT 1;
            """
            Parameters =
                [
                    "@UserId", SqliteType.Integer, 8
                    "@ChartId", SqliteType.Text, -1
                ]
            FillParameters =
                (fun p (user_id, chart_id) ->
                    p.Int64 user_id
                    p.String chart_id
                )
            Read =
                (fun r ->
                    {
                        TimePlayed = r.Int64
                        Rate = r.Float32 * 1.0f<rate>
                        Mods = r.Json JSON
                        Accuracy = r.Float64
                        Grade = r.Int32
                        Lamp = r.Int32
                        ReplayId = r.Int64Option
                    }
                )
        }

    let get_user_leaderboard_score
        (user_id: int64)
        (chart_id: string)
        : UserLeaderboardScore option =
        GET_USER_LEADERBOARD_SCORE.Execute (user_id, chart_id) core_db
        |> expect
        |> Array.tryExactlyOne

    let private AGGREGATE_USER_RANKED_GRADES: Query<int64, string * int> =
        {
            SQL =
                """
            WITH ChartBestGrades AS (
                SELECT
                    ChartId,
                    Grade,
                    ROW_NUMBER() OVER (PARTITION BY ChartId ORDER BY Accuracy DESC) AS ChartScoreRank
                FROM scores2
                WHERE UserId = @UserId AND Ranked = 1
            )
            SELECT ChartId, Grade FROM ChartBestGrades
            WHERE ChartScoreRank = 1;
            """
            Parameters = [ "@UserId", SqliteType.Integer, 8 ]
            FillParameters = fun p user_id -> p.Int64 user_id
            Read = fun r -> r.String, r.Int32
        }

    let aggregate_user_ranked_grades (user_id: int64) =
        AGGREGATE_USER_RANKED_GRADES.Execute user_id core_db |> expect

    let private AGGREGATE_USER_RANKED_SCORES: Query<int64, string * float> =
        {
            SQL =
                """
            WITH ChartBestScores AS (
                SELECT
                    ChartId,
                    Accuracy,
                    ROW_NUMBER() OVER (PARTITION BY ChartId ORDER BY Accuracy DESC) AS ChartScoreRank
                FROM scores2
                WHERE UserId = @UserId AND Ranked = 1
            )
            SELECT ChartId, Accuracy FROM ChartBestScores
            WHERE ChartScoreRank = 1;
            """
            Parameters = [ "@UserId", SqliteType.Integer, 8 ]
            FillParameters = fun p user_id -> p.Int64 user_id
            Read = fun r -> r.String, r.Float64
        }

    let aggregate_user_ranked_scores (user_id: int64) =
        AGGREGATE_USER_RANKED_SCORES.Execute user_id core_db |> expect

    type ScoreByIdModel =
        {
            UserId: int64
            ChartId: string
            TimePlayed: int64
            Rate: float32
            Mods: ModState
            Accuracy: float
            Grade: int
            Lamp: int
        }

    let BY_ID: Query<int64, ScoreByIdModel * int64 option> =
        {
            SQL =
                """
            SELECT UserId, ChartId, TimePlayed, Rate, Mods, Accuracy, Grade, Lamp, ReplayId FROM scores2
            WHERE Id = @Id
            """
            Parameters = [ "@Id", SqliteType.Integer, 8 ]
            FillParameters = fun p id -> p.Int64 id
            Read =
                (fun r ->
                    {
                        UserId = r.Int64
                        ChartId = r.String
                        TimePlayed = r.Int64
                        Rate = r.Float32
                        Mods = r.Json JSON
                        Accuracy = r.Float64
                        Grade = r.Int32
                        Lamp = r.Int32
                    },
                    r.Int64Option
                )
        }

    let by_id (score_id: int64) =
        match BY_ID.Execute score_id core_db |> expect |> Array.tryExactlyOne with
        | Some(score, Some replay_id) ->
            match Replay.by_id replay_id with
            | Some replay -> Some(score, Some replay)
            | None -> failwithf "Score %A had replay id %A, but couldn't retrieve it by id" score replay_id
        | Some(score, None) -> Some(score, None)
        | None -> None

    let WIPE_LEADERBOARD : NonQuery<string> =
        {
            SQL =
                """
            DELETE FROM replays2 WHERE ChartId = @ChartId;
            DELETE FROM scores2 WHERE ChartId = @ChartId;
            """
            Parameters = [ "@ChartId", SqliteType.Text, -1 ]
            FillParameters = fun p chart_id -> p.String chart_id
        }

    let wipe_leaderboard (chart_id: string) =
        WIPE_LEADERBOARD.Execute chart_id core_db |> expect