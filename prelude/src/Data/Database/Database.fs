namespace Prelude.Data

open System.IO
open Percyqaz.Common
open Percyqaz.Data.Sqlite
open Prelude
open Prelude.Data.Scores

type DbCell<'T>(value: 'T) =
    let mutable value = value

    member this.Value 
        with get() = value
        and internal set new_value = value <- new_value

type internal DbChartData =
    {
        Offset: Time
        LastPlayed: int64
        Comment: string
        Breakpoints: Time list
        Scores: NewScore list
        PersonalBests: Map<string, Bests>
    }

module internal DbScore =

    let internal CREATE_TABLE : NonQuery<unit> =
        { NonQuery.without_parameters() with
            SQL = """
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

    let private SAVE : NonQuery<string * NewScore> =
        {
            SQL = """
                INSERT INTO scores (ChartId, Timestamp, Replay, Rate, Mods, IsImported, Keys)
                VALUES (@ChartId, @Timestamp, @Replay, @Rate, @Mods, @IsImported, @Keys);
            """
            Parameters =
                [
                    "@ChartId", SqliteType.Text, -1
                    "@Timestamp", SqliteType.Integer, 8
                    "@Replay", SqliteType.Blob, -1
                    "@Rate", SqliteType.Real, 4
                    "@Mods", SqliteType.Text, -1
                    "@IsImported", SqliteType.Integer, 1
                    "@Keys", SqliteType.Integer, 1
                ]
            FillParameters = (fun p (chart_id, score) ->
                p.String chart_id
                p.Int64 score.Timestamp
                p.Blob score.Replay
                p.Float32 score.Rate
                p.Json JSON score.Mods
                p.Boolean score.IsImported
                p.Byte (byte score.Keys)
            )
        }
    let save (chart_id: string) (score: NewScore) (db: Database) : int64 = SAVE.ExecuteGetId (chart_id, score) db |> expect

    let save_batch (batched_scores: (string * NewScore) seq) (db: Database) = SAVE.Batch batched_scores db |> expect

    let private BY_CHART_ID : Query<string, NewScore> =
        {
            SQL = """SELECT Timestamp, Replay, Rate, Mods, IsImported, Keys FROM scores WHERE ChartId = @ChartId;"""
            Parameters = [ "@ChartId", SqliteType.Text, -1 ]
            FillParameters = fun p chart_id -> p.String chart_id
            Read = (fun r ->
                {
                    Timestamp = r.Int64
                    Replay = 
                        // todo: push into Percyqaz.Data
                        use stream = r.Stream
                        use ms = new MemoryStream()
                        stream.CopyTo ms
                        ms.ToArray()
                    Rate = r.Float32
                    Mods = r.Json JSON
                    IsImported = r.Boolean
                    Keys = int r.Byte
                }
            )
        }
    let by_chart_id (chart_id: string) (db: Database) : NewScore array = BY_CHART_ID.Execute chart_id db |> expect

    let private DELETE_BY_TIMESTAMP : NonQuery<string * int64> =
        {
            SQL = """DELETE FROM scores WHERE ChartId = @ChartId AND Timestamp = @Timestamp;"""
            Parameters = 
                [ 
                    "@ChartId", SqliteType.Text, -1
                    "@Timestamp", SqliteType.Integer, 8
                ]
            FillParameters = fun p (chart_id, timestamp) -> p.String chart_id; p.Int64 timestamp
        }
    let delete_by_timestamp (chart_id: string) (timestamp: int64) (db: Database) = DELETE_BY_TIMESTAMP.Execute (chart_id, timestamp) db |> expect

module DatabaseSetup =
    
    let migrate (db: Database) =
        db |> 
        Database.migrate
            "AddScoresTable"
            (fun db -> DbScore.CREATE_TABLE.Execute () db |> expect |> ignore)
        db

    let in_memory () =
        let db, conn = Database.in_memory "Interlude"
        migrate db, conn
