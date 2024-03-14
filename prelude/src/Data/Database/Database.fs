namespace Prelude.Data

open System.IO
open Percyqaz.Common
open Percyqaz.Data.Sqlite
open Prelude
open Prelude.Gameplay
open Prelude.Data.Scores

type DbCell<'T>(value: 'T) =
    let mutable value = value

    // todo: consider adding lock here since technically one thread can read while another writes
    member this.Value 
        with get() = value
        and internal set new_value = value <- new_value

module DbScores =

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
    let save (chart_id: string) (score: NewScore) (db: Database) : unit = SAVE.Execute (chart_id, score) db |> expect |> ignore

    let save_batch (batched_scores: (string * NewScore) seq) (db: Database) : unit = SAVE.Batch batched_scores db |> expect |> ignore

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

type DbChartData =
    {
        Offset: Time
        LastPlayed: int64
        Comment: string
        Breakpoints: Time list
        PersonalBests: Map<string, Bests>
    }

module DbChartData =

    let DEFAULT : DbChartData =
        {
            Offset = 0.0f<ms>
            LastPlayed = 0L
            Comment = ""
            Breakpoints = []
            PersonalBests = Map.empty
        }
    
    let internal CREATE_TABLE : NonQuery<unit> =
        { NonQuery.without_parameters() with
            SQL = """
            CREATE TABLE chart_data (
                ChartId TEXT PRIMARY KEY NOT NULL,
                Offset REAL NOT NULL,
                LastPlayed INTEGER NOT NULL,
                Comment TEXT NOT NULL,
                Breakpoints TEXT NOT NULL,
                PersonalBests TEXT NOT NULL
            );
            """
        }
        
    let private GET : Query<string, DbChartData> =
        {
            SQL = """SELECT Offset, LastPlayed, Comment, Breakpoints, PersonalBests FROM chart_data WHERE ChartId = @ChartId;"""
            Parameters = [ "@ChartId", SqliteType.Text, -1 ]
            FillParameters = fun p chart_id -> p.String chart_id
            Read = (fun r ->
                {
                    Offset = r.Float32 |> Time.ofFloat
                    LastPlayed = r.Int64
                    Comment = r.String
                    Breakpoints = r.Json JSON
                    PersonalBests = r.Json JSON
                }
            )
        }
    let get (chart_id: string) (db: Database) = 
        GET.Execute chart_id db
        |> expect
        |> Array.tryExactlyOne
        |> Option.defaultValue DEFAULT

    let private SAVE_OFFSET : NonQuery<string * Time> =
        {
            SQL = """
                INSERT INTO chart_data (ChartId, Offset, LastPlayed, Comment, Breakpoints, PersonalBests)
                VALUES (@ChartId, @Offset, 0, '', '[]', '{}')
                ON CONFLICT DO UPDATE SET Offset = excluded.Offset;
            """
            Parameters = 
                [ 
                    "@ChartId", SqliteType.Text, -1
                    "@Offset", SqliteType.Real, 4
                ]
            FillParameters = fun p (chart_id, offset) -> p.String chart_id; p.Float32 (float32 offset)
        }
    let save_offsets (changes: (string * Time) seq) (db: Database) = SAVE_OFFSET.Batch changes db |> expect |> ignore

    let private SAVE_LAST_PLAYED : NonQuery<string * int64> =
        {
            SQL = """
                INSERT INTO chart_data (ChartId, Offset, LastPlayed, Comment, Breakpoints, PersonalBests)
                VALUES (@ChartId, 0, @LastPlayed, '', '[]', '{}')
                ON CONFLICT DO UPDATE SET LastPlayed = excluded.LastPlayed;
            """
            Parameters = 
                [ 
                    "@ChartId", SqliteType.Text, -1
                    "@LastPlayed", SqliteType.Integer, 8
                ]
            FillParameters = fun p (chart_id, last_played) -> p.String chart_id; p.Int64 last_played
        }
    let save_last_played (changes: (string * int64) seq) (db: Database) = SAVE_LAST_PLAYED.Batch changes db |> expect |> ignore
    
    let private SAVE_COMMENT : NonQuery<string * string> =
        {
            SQL = """
                INSERT INTO chart_data (ChartId, Offset, LastPlayed, Comment, Breakpoints, PersonalBests)
                VALUES (@ChartId, 0, 0, @Comment, '[]', '{}')
                ON CONFLICT DO UPDATE SET Comment = excluded.Comment;
            """
            Parameters = 
                [ 
                    "@ChartId", SqliteType.Text, -1
                    "@Comment", SqliteType.Text, -1
                ]
            FillParameters = fun p (chart_id, comment) -> p.String chart_id; p.String comment
        }
    let save_comments (changes: (string * string) seq) (db: Database) = SAVE_COMMENT.Batch changes db |> expect |> ignore
    
    let private SAVE_BREAKPOINTS: NonQuery<string * Time list> =
        {
            SQL = """
                INSERT INTO chart_data (ChartId, Offset, LastPlayed, Comment, Breakpoints, PersonalBests)
                VALUES (@ChartId, 0, 0, '', @Breakpoints, '{}')
                ON CONFLICT DO UPDATE SET Breakpoints = excluded.Breakpoints;
            """
            Parameters = 
                [ 
                    "@ChartId", SqliteType.Text, -1
                    "@Breakpoints", SqliteType.Text, -1
                ]
            FillParameters = fun p (chart_id, breakpoints) -> p.String chart_id; p.Json JSON breakpoints
        }
    let save_breakpoints (changes: (string * Time list) seq) (db: Database) = SAVE_BREAKPOINTS.Batch changes db |> expect |> ignore

    let private SAVE_PERSONAL_BESTS: NonQuery<string * Map<string, Bests>> =
        {
            SQL = """
                INSERT INTO chart_data (ChartId, Offset, LastPlayed, Comment, Breakpoints, PersonalBests)
                VALUES (@ChartId, 0, 0, '', '[]', @PersonalBests)
                ON CONFLICT DO UPDATE SET PersonalBests = excluded.PersonalBests;
            """
            Parameters = 
                [ 
                    "@ChartId", SqliteType.Text, -1
                    "@PersonalBests", SqliteType.Text, -1
                ]
            FillParameters = fun p (chart_id, bests) -> p.String chart_id; p.Json JSON bests
        }
    let save_personal_bests (chart_id: string) (bests: Map<string, Bests>) (db: Database) = SAVE_PERSONAL_BESTS.Execute (chart_id, bests) db |> expect |> ignore

module DatabaseSetup =
    
    let private migrate (db: Database) =
        Database.migrate
            "AddScoresTable"
            (fun db -> DbScores.CREATE_TABLE.Execute () db |> expect |> ignore)
            db
        Database.migrate
            "AddChartDataTable"
            (fun db -> DbChartData.CREATE_TABLE.Execute () db |> expect |> ignore)
            db
        db

    let from_file (filename: string) = Database.from_file filename |> migrate

    let in_memory() = 
        let db, conn = Database.in_memory "interlude"
        migrate db, conn