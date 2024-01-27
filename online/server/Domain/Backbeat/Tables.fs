namespace Interlude.Web.Server.Domain.Backbeat

open Percyqaz.Common
open Percyqaz.Data
open Percyqaz.Data.Sqlite
open Prelude.Common
open Interlude.Web.Server

[<Json.AutoCodec>]
[<RequireQualifiedAccess>]
type TableChangeEventDetails =
    | Add of string * int
    | Move of string * int * int
    | Remove of string * int

type TableChangeEvent =
    {
        UserId: int64
        Timestamp: int64
        Details: TableChangeEventDetails
    }

module TableLevel =

    let internal CREATE_TABLES : NonQuery<unit> =
        { NonQuery.without_parameters() with
            SQL = """
            BEGIN TRANSACTION;

            CREATE TABLE table_levels (
                TableId TEXT NOT NULL,
                ChartId TEXT NOT NULL,
                Level INTEGER NOT NULL,
                PRIMARY KEY (TableId, ChartId)
            );

            CREATE TABLE table_changes (
                Id INTEGER PRIMARY KEY NOT NULL,
                TableId TEXT NOT NULL,
                UserId INTEGER NOT NULL,
                Timestamp INTEGER NOT NULL,
                Details TEXT NOT NULL
            );

            COMMIT;
            """
        }

    let private GET : Query<string * string, int> =
        {
            SQL = """
            SELECT Level FROM table_levels
            WHERE TableId = @TableId AND ChartId = @ChartId;
            """
            Parameters = [ "@TableId", SqliteType.Text, -1; "@ChartId", SqliteType.Text, -1 ]
            FillParameters = fun p (table_id, chart_id) -> p.String table_id; p.String chart_id
            Read = fun r -> r.Int32
        }
    let get (table_id: string) (chart_id: string) : int option = GET.Execute (table_id, chart_id) backbeat_db |> expect |> Array.tryExactlyOne

    let private GET_ALL : Query<string, string * int> =
        {
            SQL = """
            SELECT ChartId, Level FROM table_levels
            WHERE TableId = @TableId;
            """
            Parameters = [ "@TableId", SqliteType.Text, -1 ]
            FillParameters = fun p table_id -> p.String table_id
            Read = fun r -> r.String, r.Int32
        }
    let get_all (table_id: string) : (string * int) array = GET_ALL.Execute table_id backbeat_db |> expect
    
    let private GET_RANGE : Query<string * int * int, string * int> =
        {
            SQL = """
            SELECT ChartId, Level FROM table_levels
            WHERE TableId = @TableId AND Level >= @Lo AND Level <= @Hi;
            """
            Parameters = 
                [
                    "@TableId", SqliteType.Text, -1
                    "@Lo", SqliteType.Integer, 4
                    "@Hi", SqliteType.Integer, 4
                ]
            FillParameters = (fun p (table_id, lo, hi) -> 
                p.String table_id
                p.Int32 lo
                p.Int32 hi
            )
            Read = fun r -> r.String, r.Int32
        }
    let get_range (table_id: string) (lo: int) (hi: int) : (string * int) array = GET_RANGE.Execute (table_id, lo, hi) backbeat_db |> expect

    let private GET_CHANGES_SINCE : Query<string * int64, TableChangeEvent> =
        {
            SQL = """
            SELECT UserId, Timestamp, Details FROM table_changes
            WHERE TableId = @TableId AND Timestamp >= @Timestamp;
            """
            Parameters = 
                [
                    "@TableId", SqliteType.Text, -1
                    "@Timestamp", SqliteType.Integer, 8
                ]
            FillParameters = (fun p (table_id, timestamp_since) -> 
                p.String table_id
                p.Int64 timestamp_since
            )
            Read = (fun r ->
                {
                    UserId = r.Int64
                    Timestamp = r.Int64
                    Details = r.Json JSON
                }
            )
        }
    let get_changes_since (table_id: string) (timestamp_since: int64) : TableChangeEvent array =
        GET_CHANGES_SINCE.Execute (table_id, timestamp_since) backbeat_db |> expect

    let private GET_TIME_LAST_CHANGED : Query<string, int64> =
        {
            SQL = """
            SELECT Timestamp FROM table_changes
            WHERE TableId = @TableId
            ORDER BY Timestamp DESC
            LIMIT 1;
            """
            Parameters = 
                [
                    "@TableId", SqliteType.Text, -1
                ]
            FillParameters = fun p table_id -> p.String table_id
            Read = fun r -> r.Int64
        }
    let get_time_last_changed (table_id: string) : int64 option = 
        GET_TIME_LAST_CHANGED.Execute table_id backbeat_db |> expect |> Array.tryExactlyOne

    let private CHANGE_LOCK_OBJ = obj()
    let private ADD : NonQuery<{|TableId: string; ChartId: string; UserId: int64; Level: int|}> =
        {
            SQL = """
            BEGIN TRANSACTION;

            INSERT INTO table_levels (TableId, ChartId, Level)
            VALUES (@TableId, @ChartId, @Level);

            INSERT INTO table_changes (TableId, UserId, Timestamp, Details)
            VALUES (@TableId, @UserId, @Timestamp, @Details);

            COMMIT;
            """
            Parameters = 
                [
                    "@TableId", SqliteType.Text, -1
                    "@ChartId", SqliteType.Text, -1
                    "@UserId", SqliteType.Integer, 8
                    "@Level", SqliteType.Integer, 4
                    "@Timestamp", SqliteType.Integer, 8
                    "@Details", SqliteType.Text, -1
                ]
            FillParameters = (fun p m ->
                p.String m.TableId
                p.String m.ChartId
                p.Int64 m.UserId
                p.Int32 m.Level
                p.Int64 (Timestamp.now())
                p.Json JSON (TableChangeEventDetails.Add(m.ChartId, m.Level))
            )
        }
    let private MOVE : NonQuery<{|TableId: string; ChartId: string; UserId: int64; OldLevel: int; NewLevel: int|}> =
        {
            SQL = """
            BEGIN TRANSACTION;

            INSERT OR REPLACE INTO table_levels (TableId, ChartId, Level)
            VALUES (@TableId, @ChartId, @Level);

            INSERT INTO table_changes (TableId, UserId, Timestamp, Details)
            VALUES (@TableId, @UserId, @Timestamp, @Details);

            COMMIT;
            """
            Parameters = 
                [
                    "@TableId", SqliteType.Text, -1
                    "@ChartId", SqliteType.Text, -1
                    "@UserId", SqliteType.Integer, 8
                    "@Level", SqliteType.Integer, 4
                    "@Timestamp", SqliteType.Integer, 8
                    "@Details", SqliteType.Text, -1
                ]
            FillParameters = (fun p m ->
                p.String m.TableId
                p.String m.ChartId
                p.Int64 m.UserId
                p.Int32 m.NewLevel
                p.Int64 (Timestamp.now())
                p.Json JSON (TableChangeEventDetails.Move(m.ChartId, m.OldLevel, m.NewLevel))
            )
        }
    let add_or_move (user_id: int64) (table_id: string) (chart_id: string) (level: int) = lock CHANGE_LOCK_OBJ <| fun () ->
        match get table_id chart_id with
        | Some current -> 
            if current <> level then
                MOVE.Execute {| ChartId = chart_id; NewLevel = level; OldLevel = current; TableId = table_id; UserId = user_id |} backbeat_db |> expect |> ignore
        | None -> ADD.Execute {| ChartId = chart_id; Level = level; TableId = table_id; UserId = user_id |} backbeat_db |> expect |> ignore

    let private REMOVE : NonQuery<{|TableId: string; ChartId: string; UserId: int64; OldLevel: int|}> =
        {
            SQL = """
            BEGIN TRANSACTION;

            DELETE FROM table_levels
            WHERE TableId = @TableId AND ChartId = @ChartId;

            INSERT INTO table_changes (TableId, UserId, Timestamp, Details)
            VALUES (@TableId, @UserId, @Timestamp, @Details);

            COMMIT;
            """
            Parameters = 
                [
                    "@TableId", SqliteType.Text, -1
                    "@ChartId", SqliteType.Text, -1
                    "@UserId", SqliteType.Integer, 8
                    "@Level", SqliteType.Integer, 4
                    "@Timestamp", SqliteType.Integer, 8
                    "@Details", SqliteType.Text, -1
                ]
            FillParameters = (fun p m ->
                p.String m.TableId
                p.String m.ChartId
                p.Int64 m.UserId
                p.Int32 m.OldLevel
                p.Int64 (Timestamp.now())
                p.Json JSON (TableChangeEventDetails.Remove(m.ChartId, m.OldLevel))
            )
        }
    let remove (user_id: int64) (table_id: string) (chart_id: string) = lock CHANGE_LOCK_OBJ <| fun () ->
        match get table_id chart_id with
        | Some current -> 
            REMOVE.Execute {| ChartId = chart_id; OldLevel = current; TableId = table_id; UserId = user_id |} backbeat_db |> expect |> ignore
        | None -> ()