namespace Prelude.Data.User

open Percyqaz.Common
open Percyqaz.Data.Sqlite
open Prelude
open Prelude.Mods

type Score =
    {
        Timestamp: int64
        Replay: byte array
        Rate: Rate
        Mods: ModState
        IsImported: bool
        IsFailed: bool
        Keys: int
    }

module DbScores =

    let internal CREATE_TABLE: NonQuery<unit> =
        { NonQuery.without_parameters () with
            SQL =
                """
            CREATE TABLE scores (
                Id INTEGER PRIMARY KEY NOT NULL,
                ChartId TEXT NOT NULL,
                Timestamp INTEGER NOT NULL,
                Replay BLOB NOT NULL,
                Rate REAL NOT NULL,
                Mods TEXT NOT NULL,
                IsImported INTEGER NOT NULL,
                Keys INTEGER NOT NULL
            );
            """
        }
    let internal ADD_FAILED_COLUMN: NonQuery<unit> =
        { NonQuery.without_parameters () with
            SQL = """ALTER TABLE scores ADD COLUMN IsFailed INTEGER NOT NULL DEFAULT 0;"""
        }
    let internal CREATE_INDEX: NonQuery<unit> =
        { NonQuery.without_parameters () with
            SQL =
                """
            CREATE INDEX idx_scores_chartid
            ON scores (ChartId ASC);
            """
        }
    let internal CREATE_TIMESTAMP_INDEX: NonQuery<unit> =
        { NonQuery.without_parameters () with
            SQL =
                """
            CREATE INDEX idx_scores_timestamp
            ON scores (Timestamp ASC);
            """
        }

    let private SAVE: NonQuery<string * Score> =
        {
            SQL =
                """
                INSERT INTO scores (ChartId, Timestamp, Replay, Rate, Mods, IsImported, IsFailed, Keys)
                VALUES (@ChartId, @Timestamp, @Replay, @Rate, json(@Mods), @IsImported, @IsFailed, @Keys);
            """
            Parameters =
                [
                    "@ChartId", SqliteType.Text, -1
                    "@Timestamp", SqliteType.Integer, 8
                    "@Replay", SqliteType.Blob, -1
                    "@Rate", SqliteType.Real, 4
                    "@Mods", SqliteType.Text, -1
                    "@IsImported", SqliteType.Integer, 1
                    "@IsFailed", SqliteType.Integer, 1
                    "@Keys", SqliteType.Integer, 1
                ]
            FillParameters =
                (fun p (chart_id, score) ->
                    p.String chart_id
                    p.Int64 score.Timestamp
                    p.Blob score.Replay
                    p.Float32 (float32 score.Rate)
                    p.Json JSON score.Mods
                    p.Boolean score.IsImported
                    p.Boolean score.IsFailed
                    p.Byte(byte score.Keys)
                )
        }

    let save (chart_id: string) (score: Score) (db: Database) : unit =
        SAVE.Execute (chart_id, score) db |> expect |> ignore

    let save_batch (batched_scores: (string * Score) seq) (db: Database) : unit =
        SAVE.Batch batched_scores db |> expect |> ignore

    let private BY_CHART_ID: Query<string, Score> =
        {
            SQL = """SELECT Timestamp, Replay, Rate, Mods, IsImported, IsFailed, Keys FROM scores WHERE ChartId = @ChartId;"""
            Parameters = [ "@ChartId", SqliteType.Text, -1 ]
            FillParameters = fun p chart_id -> p.String chart_id
            Read =
                (fun r ->
                    {
                        Timestamp = r.Int64
                        Replay = r.Blob
                        Rate = 1.0f<rate> * r.Float32
                        Mods = r.Json JSON
                        IsImported = r.Boolean
                        IsFailed = r.Boolean
                        Keys = int r.Byte
                    }
                )
        }

    let by_chart_id (chart_id: string) (db: Database) : Score array =
        BY_CHART_ID.Execute chart_id db |> expect

    let private DELETE_BY_TIMESTAMP: NonQuery<string * int64> =
        {
            SQL = """DELETE FROM scores WHERE ChartId = @ChartId AND Timestamp = @Timestamp;"""
            Parameters = [ "@ChartId", SqliteType.Text, -1; "@Timestamp", SqliteType.Integer, 8 ]
            FillParameters =
                fun p (chart_id, timestamp) ->
                    p.String chart_id
                    p.Int64 timestamp
        }

    let delete_by_timestamp (chart_id: string) (timestamp: int64) (db: Database) =
        DELETE_BY_TIMESTAMP.Execute (chart_id, timestamp) db |> expect

    let private FAST_LOAD: Query<int, string * Score> =
        {
            SQL =
                """
            SELECT ChartId, Timestamp, Replay, Rate, Mods, IsImported, IsFailed, Keys FROM scores
            ORDER BY ChartId ASC
            LIMIT 1000
            OFFSET @Offset;
            """
            Parameters = [ "@Offset", SqliteType.Integer, 4 ]
            FillParameters = fun p page -> p.Int32(page * 1000)
            Read =
                (fun r ->
                    r.String,
                    {
                        Timestamp = r.Int64
                        Replay = r.Blob
                        Rate = 1.0f<rate> * r.Float32
                        Mods = r.Json JSON
                        IsImported = r.Boolean
                        IsFailed = r.Boolean
                        Keys = int r.Byte
                    }
                )
        }

    let fast_load (db: Database) : (string * Score list) seq =
        let all_scores =
            seq {
                let mutable batch = 0
                let mutable next_batch = FAST_LOAD.Execute batch db |> expect
                yield! next_batch

                while next_batch.Length = 1000 do
                    batch <- batch + 1
                    next_batch <- FAST_LOAD.Execute batch db |> expect
                    yield! next_batch
            }

        seq {
            let mutable current_chart = ""
            let mutable current = []

            for chart_id, score in all_scores do
                if chart_id <> current_chart then
                    if current_chart <> "" then
                        yield current_chart, current

                    current <- []
                    current_chart <- chart_id

                current <- score :: current

            if current_chart <> "" then
                yield current_chart, current
        }

    let private GET_BETWEEN: Query<int64 * int64, string * Score> =
        {
            SQL =
                """
            SELECT ChartId, Timestamp, Replay, Rate, Mods, IsImported, IsFailed, Keys FROM scores
            WHERE Timestamp >= @StartTime AND Timestamp <= @EndTime
            ORDER BY Timestamp ASC;
            """
            Parameters = [ "@StartTime", SqliteType.Integer, 8; "@EndTime", SqliteType.Integer, 8 ]
            FillParameters = fun p (start_time, end_time) -> p.Int64 start_time; p.Int64 end_time
            Read =
                (fun r ->
                    r.String,
                    {
                        Timestamp = r.Int64
                        Replay = r.Blob
                        Rate = 1.0f<rate> * r.Float32
                        Mods = r.Json JSON
                        IsImported = r.Boolean
                        IsFailed = r.Boolean
                        Keys = int r.Byte
                    }
                )
        }

    let get_between (start_time: int64) (end_time: int64) (db: Database) : (string * Score) array =
        GET_BETWEEN.Execute (start_time, end_time) db |> expect

    let private TRANSFER: NonQuery<string * string> =
        {
            SQL =
                """
            UPDATE scores SET ChartId = @AfterHash WHERE ChartId = @BeforeHash;
            """
            Parameters = [ "@BeforeHash", SqliteType.Text, -1; "@AfterHash", SqliteType.Text, -1 ]
            FillParameters = fun p (before_hash, after_hash) -> p.String before_hash; p.String after_hash
        }

    let transfer (before_hash: string) (after_hash: string) (db: Database) : int =
        TRANSFER.Execute (before_hash, after_hash) db |> expect