namespace Interlude.Web.Server.Domain.Backbeat

open Percyqaz.Common
open Percyqaz.Data
open Percyqaz.Data.Sqlite
open Prelude
open Interlude.Web.Server

[<Json.AutoCodec>]
[<RequireQualifiedAccess>]
type TableChangeEventDetails =
    | Add of int
    | Move of int * int
    | Remove of int

type TableChangeEvent =
    {
        ChartId: string
        UserId: int64
        Timestamp: int64
        Details: TableChangeEventDetails
    }

module TableLevel =

    let internal CREATE_TABLES: NonQuery<unit> =
        { NonQuery.without_parameters () with
            SQL =
                """
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
                ChartId TEXT NOT NULL,
                UserId INTEGER NOT NULL,
                Timestamp INTEGER NOT NULL,
                Details TEXT NOT NULL
            );

            COMMIT;
            """
        }

    let private GET: Query<string * string, int> =
        {
            SQL =
                """
            SELECT Level FROM table_levels
            WHERE TableId = @TableId AND ChartId = @ChartId;
            """
            Parameters = [ "@TableId", SqliteType.Text, -1; "@ChartId", SqliteType.Text, -1 ]
            FillParameters =
                fun p (table_id, chart_id) ->
                    p.String table_id
                    p.String chart_id
            Read = fun r -> r.Int32
        }

    let get (table_id: string) (chart_id: string) : int option =
        GET.Execute (table_id, chart_id) backbeat_db |> expect |> Array.tryExactlyOne

    let private GET_ALL: Query<string, string * int> =
        {
            SQL =
                """
            SELECT ChartId, Level FROM table_levels
            WHERE TableId = @TableId;
            """
            Parameters = [ "@TableId", SqliteType.Text, -1 ]
            FillParameters = fun p table_id -> p.String table_id
            Read = fun r -> r.String, r.Int32
        }

    let get_all (table_id: string) : (string * int) array =
        GET_ALL.Execute table_id backbeat_db |> expect

    let private GET_RANGE: Query<string * int * int, string * int> =
        {
            SQL =
                """
            SELECT ChartId, Level FROM table_levels
            WHERE TableId = @TableId AND Level >= @Lo AND Level <= @Hi;
            """
            Parameters =
                [
                    "@TableId", SqliteType.Text, -1
                    "@Lo", SqliteType.Integer, 4
                    "@Hi", SqliteType.Integer, 4
                ]
            FillParameters =
                (fun p (table_id, lo, hi) ->
                    p.String table_id
                    p.Int32 lo
                    p.Int32 hi
                )
            Read = fun r -> r.String, r.Int32
        }

    let get_range (table_id: string) (lo: int) (hi: int) : (string * int) array =
        GET_RANGE.Execute (table_id, lo, hi) backbeat_db |> expect

    let private GET_CHANGES_SINCE: Query<string * int64, TableChangeEvent> =
        {
            SQL =
                """
            SELECT ChartId, UserId, Timestamp, Details FROM table_changes
            WHERE TableId = @TableId AND Timestamp >= @Timestamp;
            """
            Parameters = [ "@TableId", SqliteType.Text, -1; "@Timestamp", SqliteType.Integer, 8 ]
            FillParameters =
                (fun p (table_id, timestamp_since) ->
                    p.String table_id
                    p.Int64 timestamp_since
                )
            Read =
                (fun r ->
                    {
                        ChartId = r.String
                        UserId = r.Int64
                        Timestamp = r.Int64
                        Details = r.Json JSON
                    }
                )
        }

    let get_changes_since (table_id: string) (timestamp_since: int64) : TableChangeEvent array =
        GET_CHANGES_SINCE.Execute (table_id, timestamp_since) backbeat_db |> expect

    let private GET_TIME_LAST_CHANGED: Query<string, int64> =
        {
            SQL =
                """
            SELECT Timestamp FROM table_changes
            WHERE TableId = @TableId
            ORDER BY Timestamp DESC
            LIMIT 1;
            """
            Parameters = [ "@TableId", SqliteType.Text, -1 ]
            FillParameters = fun p table_id -> p.String table_id
            Read = fun r -> r.Int64
        }

    let get_time_last_changed (table_id: string) : int64 option =
        GET_TIME_LAST_CHANGED.Execute table_id backbeat_db
        |> expect
        |> Array.tryExactlyOne

    let private CHANGE_LOCK_OBJ = obj ()

    let private ADD
        : NonQuery<{|
              TableId: string
              ChartId: string
              UserId: int64
              Level: int
          |}> =
        {
            SQL =
                """
            BEGIN TRANSACTION;

            INSERT INTO table_levels (TableId, ChartId, Level)
            VALUES (@TableId, @ChartId, @Level);

            INSERT INTO table_changes (TableId, ChartId, UserId, Timestamp, Details)
            VALUES (@TableId, @ChartId, @UserId, @Timestamp, @Details);

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
            FillParameters =
                (fun p m ->
                    p.String m.TableId
                    p.String m.ChartId
                    p.Int64 m.UserId
                    p.Int32 m.Level
                    p.Int64(Timestamp.now ())
                    p.Json JSON (TableChangeEventDetails.Add m.Level)
                )
        }

    let private MOVE
        : NonQuery<{|
              TableId: string
              ChartId: string
              UserId: int64
              OldLevel: int
              NewLevel: int
          |}> =
        {
            SQL =
                """
            BEGIN TRANSACTION;

            INSERT OR REPLACE INTO table_levels (TableId, ChartId, Level)
            VALUES (@TableId, @ChartId, @Level);

            INSERT INTO table_changes (TableId, ChartId, UserId, Timestamp, Details)
            VALUES (@TableId, @ChartId, @UserId, @Timestamp, @Details);

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
            FillParameters =
                (fun p m ->
                    p.String m.TableId
                    p.String m.ChartId
                    p.Int64 m.UserId
                    p.Int32 m.NewLevel
                    p.Int64(Timestamp.now ())
                    p.Json JSON (TableChangeEventDetails.Move(m.OldLevel, m.NewLevel))
                )
        }

    let add_or_move (user_id: int64) (table_id: string) (chart_id: string) (level: int) =
        lock CHANGE_LOCK_OBJ
        <| fun () ->
            match get table_id chart_id with
            | Some current ->
                if current <> level then
                    MOVE.Execute
                        {|
                            ChartId = chart_id
                            NewLevel = level
                            OldLevel = current
                            TableId = table_id
                            UserId = user_id
                        |}
                        backbeat_db
                    |> expect
                    |> ignore
            | None ->
                ADD.Execute
                    {|
                        ChartId = chart_id
                        Level = level
                        TableId = table_id
                        UserId = user_id
                    |}
                    backbeat_db
                |> expect
                |> ignore

    let private REMOVE
        : NonQuery<{|
              TableId: string
              ChartId: string
              UserId: int64
              OldLevel: int
          |}> =
        {
            SQL =
                """
            BEGIN TRANSACTION;

            DELETE FROM table_levels
            WHERE TableId = @TableId AND ChartId = @ChartId;

            INSERT INTO table_changes (TableId, ChartId, UserId, Timestamp, Details)
            VALUES (@TableId, @ChartId, @UserId, @Timestamp, @Details);

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
            FillParameters =
                (fun p m ->
                    p.String m.TableId
                    p.String m.ChartId
                    p.Int64 m.UserId
                    p.Int32 m.OldLevel
                    p.Int64(Timestamp.now ())
                    p.Json JSON (TableChangeEventDetails.Remove m.OldLevel)
                )
        }

    let remove (user_id: int64) (table_id: string) (chart_id: string) =
        lock CHANGE_LOCK_OBJ
        <| fun () ->
            match get table_id chart_id with
            | Some current ->
                REMOVE.Execute
                    {|
                        ChartId = chart_id
                        OldLevel = current
                        TableId = table_id
                        UserId = user_id
                    |}
                    backbeat_db
                |> expect
                |> ignore
            | None -> ()

[<Json.AutoCodec>]
[<RequireQualifiedAccess>]
type TableSuggestionStatus =
    | Pending
    | Accepted of timestamp: int64 * user_id: int64 * level: int
    | Rejected of timestamp: int64 * user_id: int64 * reason: string

type TableSuggestion =
    {
        TableId: string
        ChartId: string
        UserId: int64
        TimeSuggested: int64
        Votes: Map<int64, int>
        Status: TableSuggestionStatus
    }

module TableSuggestion =

    let internal CREATE_TABLE: NonQuery<unit> =
        { NonQuery.without_parameters () with
            SQL =
                """
            CREATE TABLE table_suggestions (
                Id INTEGER PRIMARY KEY NOT NULL,
                TableId TEXT NOT NULL,
                ChartId TEXT NOT NULL,
                UserId INTEGER NOT NULL,
                TimeSuggested INTEGER NOT NULL,
                Votes TEXT NOT NULL,
                Status TEXT NOT NULL
            );
            """
        }

    let private PENDING_BY_CHART: Query<string * string, Map<int64, int>> =
        {
            SQL =
                """
            SELECT Votes FROM table_suggestions
            WHERE TableId = @TableId AND ChartId = @ChartId AND Status = '"Pending"';
            """
            Parameters = [ "@TableId", SqliteType.Text, -1; "@ChartId", SqliteType.Text, -1 ]
            FillParameters =
                fun p (table_id, chart_id) ->
                    p.String table_id
                    p.String chart_id
            Read = fun r -> r.Json JSON
        }

    let pending_by_chart (table_id: string) (chart_id: string) : Map<int64, int> option =
        PENDING_BY_CHART.Execute (table_id, chart_id) backbeat_db
        |> expect
        |> Array.tryExactlyOne

    type TableChartSuggestionModel =
        {
            UserId: int64
            TimeSuggested: int64
            Votes: Map<int64, int>
            Status: TableSuggestionStatus
        }

    let private ALL_BY_CHART: Query<string * string, TableChartSuggestionModel> =
        {
            SQL =
                """
            SELECT UserId, TimeSuggested, Votes, Status FROM table_suggestions
            WHERE TableId = @TableId AND ChartId = @ChartId;
            """
            Parameters = [ "@TableId", SqliteType.Text, -1; "@ChartId", SqliteType.Text, -1 ]
            FillParameters =
                fun p (table_id, chart_id) ->
                    p.String table_id
                    p.String chart_id
            Read =
                (fun r ->
                    {
                        UserId = r.Int64
                        TimeSuggested = r.Int64
                        Votes = r.Json JSON
                        Status = r.Json JSON
                    }
                )
        }

    let all_by_chart (table_id: string) (chart_id: string) : TableChartSuggestionModel array =
        ALL_BY_CHART.Execute (table_id, chart_id) backbeat_db |> expect

    type TableSuggestionModel =
        {
            ChartId: string
            UserId: int64
            TimeSuggested: int64
            Votes: Map<int64, int>
        }

    let private PENDING_BY_TABLE: Query<string, TableSuggestionModel> =
        {
            SQL =
                """
            SELECT ChartId, UserId, TimeSuggested, Votes FROM table_suggestions
            WHERE TableId = @TableId AND Status = '"Pending"'
            ORDER BY TimeSuggested ASC;
            """
            Parameters = [ "@TableId", SqliteType.Text, -1 ]
            FillParameters = fun p table_id -> p.String table_id
            Read =
                (fun r ->
                    {
                        ChartId = r.String
                        UserId = r.Int64
                        TimeSuggested = r.Int64
                        Votes = r.Json JSON
                    }
                )
        }
    // todo: paging
    let pending_by_table (table_id: string) : TableSuggestionModel array =
        PENDING_BY_TABLE.Execute table_id backbeat_db |> expect

    // todo: pending_by_user, all_by_user

    let private SUGGESTIONS_LOCK_OBJ = obj ()

    let private SUGGEST_NEW: NonQuery<string * string * int64 * int> =
        {
            SQL =
                """
            INSERT INTO table_suggestions (TableId, ChartId, UserId, TimeSuggested, Votes, Status)
            VALUES (@TableId, @ChartId, @UserId, @TimeSuggested, @Votes, @Status);
            """
            Parameters =
                [
                    "@TableId", SqliteType.Text, -1
                    "@ChartId", SqliteType.Text, -1
                    "@UserId", SqliteType.Integer, 8
                    "@TimeSuggested", SqliteType.Integer, 8
                    "@Votes", SqliteType.Text, -1
                    "@Status", SqliteType.Text, -1
                ]
            FillParameters =
                (fun p (table_id, chart_id, user_id, level) ->
                    p.String table_id
                    p.String chart_id
                    p.Int64 user_id
                    p.Int64(Timestamp.now ())
                    p.Json JSON (Map.ofList [ user_id, level ])
                    p.Json JSON TableSuggestionStatus.Pending
                )
        }

    let private SUGGEST_VOTE: NonQuery<string * string * Map<int64, int>> =
        {
            SQL =
                """
            UPDATE table_suggestions
            SET Votes = @Votes
            WHERE TableId = @TableId AND ChartId = @ChartId AND Status = '"Pending"';
            """
            Parameters =
                [
                    "@TableId", SqliteType.Text, -1
                    "@ChartId", SqliteType.Text, -1
                    "@Votes", SqliteType.Text, -1
                ]
            FillParameters =
                (fun p (table_id, chart_id, new_votes) ->
                    p.String table_id
                    p.String chart_id
                    p.Json JSON new_votes
                )
        }

    let suggest (table_id: string) (chart_id: string) (user_id: int64) (level: int) : bool =
        lock SUGGESTIONS_LOCK_OBJ
        <| fun () ->
            let all_suggestions_including_resolved = all_by_chart table_id chart_id

            match
                all_suggestions_including_resolved
                |> Array.tryPick (fun x ->
                    match x.Status with
                    | TableSuggestionStatus.Rejected(timestamp, user_id, reason) -> Some(timestamp, user_id, reason)
                    | _ -> None
                )
            with
            | Some(timestamp, user_id, reason) -> false
            | None ->

            match
                all_suggestions_including_resolved
                |> Array.tryFind (fun x -> x.Status = TableSuggestionStatus.Pending)
            with
            | Some pending ->
                let new_votes = Map.add user_id level pending.Votes

                if new_votes <> pending.Votes then
                    SUGGEST_VOTE.Execute (table_id, chart_id, new_votes) backbeat_db
                    |> expect
                    |> ignore
            | None ->
                SUGGEST_NEW.Execute (table_id, chart_id, user_id, level) backbeat_db
                |> expect
                |> ignore

            true

    let suggest_allow_reopening_rejected (table_id: string) (chart_id: string) (user_id: int64) (level: int) =
        lock SUGGESTIONS_LOCK_OBJ
        <| fun () ->
            match pending_by_chart table_id chart_id with
            | Some votes ->
                let new_votes = Map.add user_id level votes

                if new_votes <> votes then
                    SUGGEST_VOTE.Execute (table_id, chart_id, new_votes) backbeat_db
                    |> expect
                    |> ignore
            | None ->
                SUGGEST_NEW.Execute (table_id, chart_id, user_id, level) backbeat_db
                |> expect
                |> ignore

    let private ACCEPT: NonQuery<string * string * int64 * int> =
        {
            SQL =
                """
            UPDATE table_suggestions
            SET Status = @Status
            WHERE TableId = @TableId AND ChartId = @ChartId AND Status = '"Pending"';
            """
            Parameters =
                [
                    "@TableId", SqliteType.Text, -1
                    "@ChartId", SqliteType.Text, -1
                    "@Status", SqliteType.Text, -1
                ]
            FillParameters =
                (fun p (table_id, chart_id, user_id, level) ->
                    p.String table_id
                    p.String chart_id
                    p.Json JSON (TableSuggestionStatus.Accepted(Timestamp.now (), user_id, level))
                )
        }

    let accept (table_id: string) (chart_id: string) (user_id: int64) (level: int) : bool =
        lock SUGGESTIONS_LOCK_OBJ
        <| fun () ->
            match pending_by_chart table_id chart_id with
            | Some _ ->
                ACCEPT.Execute (table_id, chart_id, user_id, level) backbeat_db
                |> expect
                |> ignore

                TableLevel.add_or_move user_id table_id chart_id level
                true
            | None -> false

    let private REJECT: NonQuery<string * string * int64 * string> =
        {
            SQL =
                """
            UPDATE table_suggestions
            SET Status = @Status
            WHERE TableId = @TableId AND ChartId = @ChartId AND Status = '"Pending"';
            """
            Parameters =
                [
                    "@TableId", SqliteType.Text, -1
                    "@ChartId", SqliteType.Text, -1
                    "@Status", SqliteType.Text, -1
                ]
            FillParameters =
                (fun p (table_id, chart_id, user_id, reason) ->
                    p.String table_id
                    p.String chart_id
                    p.Json JSON (TableSuggestionStatus.Rejected(Timestamp.now (), user_id, reason))
                )
        }

    let reject (table_id: string) (chart_id: string) (user_id: int64) (reason: string) : bool =
        lock SUGGESTIONS_LOCK_OBJ
        <| fun () ->
            match pending_by_chart table_id chart_id with
            | Some _ ->
                REJECT.Execute (table_id, chart_id, user_id, reason) backbeat_db
                |> expect
                |> ignore

                true
            | None -> false
