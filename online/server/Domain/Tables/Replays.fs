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
        Data: byte array
    }

module Replay =

    let internal CREATE_TABLE : NonQuery<unit> =
        { NonQuery.without_parameters() with
            SQL = """
            CREATE TABLE replays (
                Id INTEGER PRIMARY KEY NOT NULL,
                UserId INTEGER NOT NULL,
                ChartId TEXT NOT NULL,
                Purposes TEXT NOT NULL,
                TimePlayed INTEGER NOT NULL,
                TimeUploaded INTEGER NOT NULL,
                Data BLOB NOT NULL,
                FOREIGN KEY (UserId) REFERENCES users(Id) ON DELETE CASCADE,
                UNIQUE (UserId, ChartId, TimePlayed)
            );
            """
        }

    let create (user_id: int64, chart_id: string, timestamp: int64, replay: ReplayData) =
        {
            UserId = user_id
            ChartId = chart_id
            TimePlayed = timestamp
            TimeUploaded = DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()
            Data = Replay.compress_bytes replay
        }

    let private GET_LEADERBOARD : Query<{| UserId: int64; ChartId: string; RulesetId: string |}, {| Id: int64; Purposes: Set<string>; TimePlayed: int64 |}> =
        {
            SQL = """
                SELECT Id, Purposes, TimePlayed FROM replays
                WHERE UserId = @UserId
                AND ChartId = @ChartId
                AND Purposes LIKE @Pattern ESCAPE '\';
            """
            Parameters = 
                [
                    "@UserId", SqliteType.Integer, 8
                    "@ChartId", SqliteType.Text, -1
                    "@Pattern", SqliteType.Text, -1
                ]
            FillParameters = (fun p req ->
                p.Int64 req.UserId
                p.String req.ChartId
                p.String ("%" + req.RulesetId.Replace("_", "\\_").Replace("%", "\\%") + "%")
            )
            Read = (fun r ->
                {|
                    Id = r.Int64
                    Purposes = r.Json JSON
                    TimePlayed = r.Int64
                |}
            )
        }
    let private UPDATE_PURPOSES : NonQuery<int64 * Set<string>> =
        {
            SQL = """
                UPDATE replays
                SET Purposes = @Purposes
                WHERE Id = @Id;
            """
            Parameters = 
                [
                    "@Id", SqliteType.Integer, 8
                    "@Purposes", SqliteType.Text, -1
                ]
            FillParameters = (fun p (id, purposes) ->
                p.Int64 id
                p.Json JSON purposes
            )
        }
    let private DELETE_BY_ID : NonQuery<int64> =
        {
            SQL = "DELETE FROM replays WHERE Id = @Id;"
            Parameters = [ "@Id", SqliteType.Integer, 8 ]
            FillParameters = fun p id -> p.Int64 id
        }
    let private SAVE_LEADERBOARD : Query<string * Replay, int64> =
        {
            SQL = """
            INSERT INTO replays (UserId, ChartId, Purposes, TimePlayed, TimeUploaded, Data)
            VALUES (@UserId, @ChartId, @PurposeSingleton, @TimePlayed, @TimeUploaded, @Data)
            ON CONFLICT DO UPDATE SET 
                TimeUploaded = excluded.TimeUploaded,
                Purposes = (
                    SELECT json_group_array(value) FROM (
                        SELECT json_each.value
                        FROM replays, json_each(replays.Purposes)
                        WHERE replays.Id = excluded.Id
                        UNION ALL SELECT @Purpose
                    ) GROUP BY ''
                )
            RETURNING Id;
            """
            Parameters =
                [
                    "@UserId", SqliteType.Integer, 8
                    "@ChartId", SqliteType.Text, -1
                    "@TimePlayed", SqliteType.Integer, 8
                    "@TimeUploaded", SqliteType.Integer, 8
                    "@Data", SqliteType.Blob, -1
                    "@PurposeSingleton", SqliteType.Text, -1
                    "@Purpose", SqliteType.Text, -1
                ]
            FillParameters = (fun p (ruleset, replay) ->
                p.Int64 replay.UserId
                p.String replay.ChartId
                p.Int64 replay.TimePlayed
                p.Int64 replay.TimeUploaded
                p.Blob replay.Data
                p.Json JSON [ ruleset ]
                p.String ruleset
            )
            Read = fun r -> r.Int64
        }
    // Only the most best replay per-chart is stored, overwriting any previous replay for that chart
    let save_leaderboard (ruleset_id: string) (replay: Replay) =
        match GET_LEADERBOARD.Execute {| ChartId = replay.ChartId; RulesetId = ruleset_id; UserId = replay.UserId |} db |> expect |> Array.tryExactlyOne with
        | Some existing ->
            // If exact replay already exists in DB
            if existing.TimePlayed = replay.TimePlayed then existing.Id else 
            
            // If a score for this chart, this ruleset, already exists, with only this purpose, delete it
            if existing.Purposes.Count = 1 then
                DELETE_BY_ID.Execute existing.Id db |> expect |> ignore
            // Otherwise remove that purpose and keep it
            else
                UPDATE_PURPOSES.Execute (existing.Id, Set.remove ruleset_id existing.Purposes) db |> expect |> ignore
            
            SAVE_LEADERBOARD.Execute (ruleset_id, replay) db |> expect |> Array.exactlyOne

        | None -> 
            SAVE_LEADERBOARD.Execute (ruleset_id, replay) db |> expect |> Array.exactlyOne

    let private SAVE_CHALLENGE : Query<Replay, int64> =
        {
            SQL = """
            INSERT INTO replays (UserId, ChartId, Purposes, TimePlayed, TimeUploaded, Data)
            VALUES (@UserId, @ChartId, '["challenge"]', @TimePlayed, @TimeUploaded, @Data)
            ON CONFLICT DO UPDATE SET 
                TimeUploaded = excluded.TimeUploaded,
                Purposes = (
                    SELECT json_group_array(value) FROM (
                        SELECT json_each.value
                        FROM replays, json_each(replays.Purposes)
                        WHERE replays.Id = excluded.Id
                        UNION ALL SELECT 'challenge'
                    ) GROUP BY ''
                )
            RETURNING Id;
            """
            Parameters =
                [
                    "@UserId", SqliteType.Integer, 8
                    "@ChartId", SqliteType.Text, -1
                    "@TimePlayed", SqliteType.Integer, 8
                    "@TimeUploaded", SqliteType.Integer, 8
                    "@Data", SqliteType.Blob, -1
                ]
            FillParameters = (fun p replay ->
                p.Int64 replay.UserId
                p.String replay.ChartId
                p.Int64 replay.TimePlayed
                p.Int64 replay.TimeUploaded
                p.Blob replay.Data
            )
            Read = fun r -> r.Int64
        }
    // Replay is stored long term for sharing with friends
    let save_challenge (replay: Replay) = SAVE_CHALLENGE.Execute replay db |> expect |> Array.exactlyOne

    let private BY_ID : Query<int64, Replay> =
        {
            SQL = """
            SELECT Id, UserId, ChartId, TimePlayed, TimeUploaded, Data FROM replays
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
                    Data =
                        // todo: push into Percyqaz.Data
                        use stream = r.Stream
                        use ms = new IO.MemoryStream()
                        stream.CopyTo ms
                        ms.ToArray()
                }
            )
        }
    let by_id (replay_id: int64) = BY_ID.Execute replay_id db |> expect |> Array.tryExactlyOne