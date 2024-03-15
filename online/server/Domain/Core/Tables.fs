namespace Interlude.Web.Server.Domain.Core

open Percyqaz.Common
open Percyqaz.Data.Sqlite
open Interlude.Web.Server

module TableRating =

    let internal CREATE_TABLE: NonQuery<unit> =
        { NonQuery.without_parameters () with
            SQL =
                """
            CREATE TABLE table_ratings (
                UserId INTEGER NOT NULL,
                TableId TEXT NOT NULL,
                Rating REAL NOT NULL,
                TimeCalculated INTEGER NOT NULL,
                PRIMARY KEY (UserId, TableId),
                FOREIGN KEY (UserId) REFERENCES users(Id) ON DELETE CASCADE
            );
            """
        }

    let private GET: Query<int64 * string, float * int64> =
        {
            SQL =
                """
            SELECT Rating, TimeCalculated FROM table_ratings
            WHERE UserId = @UserId AND TableId = @TableId;
            """
            Parameters = [ "@UserId", SqliteType.Integer, 8; "@TableId", SqliteType.Text, -1 ]
            FillParameters =
                fun p (user_id, table_id) ->
                    p.Int64 user_id
                    p.String table_id
            Read = fun r -> r.Float64, r.Int64
        }

    let get (user_id: int64) (table_id: string) =
        GET.Execute (user_id, table_id) core_db |> expect |> Array.tryExactlyOne

    let private SET: NonQuery<int64 * string * float> =
        {
            SQL =
                """
            INSERT OR REPLACE INTO table_ratings (UserId, TableId, Rating, TimeCalculated)
            VALUES (@UserId, @TableId, @Rating, @TimeCalculated);
            """
            Parameters =
                [
                    "@UserId", SqliteType.Integer, 8
                    "@TableId", SqliteType.Text, -1
                    "@Rating", SqliteType.Real, 8
                    "@TimeCalculated", SqliteType.Integer, 8
                ]
            FillParameters =
                (fun p (user_id, table_id, rating) ->
                    p.Int64 user_id
                    p.String table_id
                    p.Float64 rating
                    p.Int64(Timestamp.now ())
                )
        }

    let set (user_id: int64) (table_id: string) (rating: float) =
        SET.Execute (user_id, table_id, rating) core_db |> expect |> ignore

    type TableLeaderboardModel = { UserId: int64; Rating: float }

    let private LEADERBOARD: Query<string, TableLeaderboardModel> =
        {
            SQL =
                """
            SELECT UserId, Rating FROM table_ratings
            WHERE TableId = @TableId
            ORDER BY Rating DESC
            LIMIT 50;
            """
            Parameters = [ "@TableId", SqliteType.Text, -1 ]
            FillParameters = fun p table_id -> p.String table_id
            Read = (fun r -> { UserId = r.Int64; Rating = r.Float64 })
        }

    let leaderboard (table_id: string) =
        LEADERBOARD.Execute table_id core_db |> expect
