namespace Interlude.Web.Server.Domain.Core

open Percyqaz.Common
open Percyqaz.Data.Sqlite
open Prelude.Gameplay.Replays
open Interlude.Web.Server

type Replay_Prelude = Replay

type Replay =
    {
        UserId: int64
        ChartId: string
        TimePlayed: int64
        TimeUploaded: int64
        Data: byte array
    }

module Replay =

    let internal CREATE_TABLE: NonQuery<unit> =
        { NonQuery.without_parameters () with
            SQL =
                """
            CREATE TABLE replays2 (
                Id INTEGER PRIMARY KEY NOT NULL,
                UserId INTEGER NOT NULL,
                ChartId TEXT NOT NULL,
                Persistent INTEGER NOT NULL,
                TimePlayed INTEGER NOT NULL,
                TimeUploaded INTEGER NOT NULL,
                Data BLOB NOT NULL,
                FOREIGN KEY (UserId) REFERENCES users(Id) ON DELETE CASCADE,
                UNIQUE (UserId, ChartId, TimePlayed)
            );
            """
        }
        
    let internal REMOVE_PERSISTENT_COLUMN: NonQuery<unit> =
        { NonQuery.without_parameters() with
            SQL =
                """
            ALTER TABLE replays2 DROP COLUMN Persistent;
                """
        }

    let create (user_id: int64, chart_id: string, timestamp: int64, replay: Replay_Prelude) : Replay =
        {
            UserId = user_id
            ChartId = chart_id
            TimePlayed = timestamp
            TimeUploaded = Timestamp.now ()
            Data = replay.ToByteArray()
        }

    let private SAVE: Query<Replay, int64> =
        {
            SQL =
                """
            INSERT INTO replays2 (UserId, ChartId, TimePlayed, TimeUploaded, Data)
            VALUES (@UserId, @ChartId, @TimePlayed, @TimeUploaded, @Data)
            ON CONFLICT DO UPDATE SET TimeUploaded = excluded.TimeUploaded
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
            FillParameters =
                (fun p replay ->
                    p.Int64 replay.UserId
                    p.String replay.ChartId
                    p.Int64 replay.TimePlayed
                    p.Int64 replay.TimeUploaded
                    p.Blob replay.Data
                )
            Read = fun r -> r.Int64
        }

    let save (replay: Replay) =
        SAVE.Execute replay core_db |> expect |> Array.exactlyOne

    let private BY_ID: Query<int64, Replay> =
        {
            SQL =
                """
            SELECT UserId, ChartId, TimePlayed, TimeUploaded, Data FROM replays2
            WHERE Id = @Id;
            """
            Parameters = [ "@Id", SqliteType.Integer, 8 ]
            FillParameters = fun p id -> p.Int64 id
            Read =
                (fun r ->
                    {
                        UserId = r.Int64
                        ChartId = r.String
                        TimePlayed = r.Int64
                        TimeUploaded = r.Int64
                        Data = r.Blob
                    }
                )
        }

    let by_id (replay_id: int64) =
        BY_ID.Execute replay_id core_db |> expect |> Array.tryExactlyOne

    let by_ids (replay_ids: int64 array) =
        if replay_ids.Length = 0 then
            [||]
        else

        let ids_string = String.concat "," (Array.map string replay_ids)

        let query: Query<unit, int64 * Replay> =
            { Query.without_parameters () with
                SQL =
                    sprintf
                        "SELECT Id, UserId, ChartId, TimePlayed, TimeUploaded, Data FROM replays2 WHERE Id IN (%s)"
                        ids_string
                Read =
                    (fun r ->
                        r.Int64,
                        {
                            UserId = r.Int64
                            ChartId = r.String
                            TimePlayed = r.Int64
                            TimeUploaded = r.Int64
                            Data = r.Blob
                        }
                    )
            }

        query.Execute () core_db |> expect