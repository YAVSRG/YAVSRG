namespace Prelude.Data.User

open Percyqaz.Common
open Percyqaz.Data.Sqlite
open Prelude

type DbChartData =
    {
        Offset: Time
        LastPlayed: int64
        Comment: string
        Breakpoints: Time list
        PersonalBests: Map<string, Bests>
    }

module DbChartData =

    let DEFAULT: DbChartData =
        {
            Offset = 0.0f<ms>
            LastPlayed = 0L
            Comment = ""
            Breakpoints = []
            PersonalBests = Map.empty
        }

    let internal CREATE_TABLE: NonQuery<unit> =
        { NonQuery.without_parameters () with
            SQL =
                """
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

    let internal RESET_PERSONAL_BESTS: NonQuery<unit> =
        { NonQuery.without_parameters () with
            SQL = """ UPDATE chart_data SET PersonalBests = '[]'; """
        }

    let private GET: Query<string, DbChartData> =
        {
            SQL =
                """SELECT Offset, LastPlayed, Comment, Breakpoints, PersonalBests FROM chart_data WHERE ChartId = @ChartId;"""
            Parameters = [ "@ChartId", SqliteType.Text, -1 ]
            FillParameters = fun p chart_id -> p.String chart_id
            Read =
                (fun r ->
                    {
                        Offset = r.Float32 |> Time.of_number
                        LastPlayed = r.Int64
                        Comment = r.String
                        Breakpoints = r.Json JSON
                        PersonalBests = r.Json JSON
                    }
                )
        }

    let get (chart_id: string) (db: Database) : DbChartData =
        GET.Execute chart_id db
        |> expect
        |> Array.tryExactlyOne
        |> Option.defaultValue DEFAULT

    let private SAVE_OFFSET: NonQuery<string * Time> =
        {
            SQL =
                """
                INSERT INTO chart_data (ChartId, Offset, LastPlayed, Comment, Breakpoints, PersonalBests)
                VALUES (@ChartId, @Offset, 0, '', '[]', '{}')
                ON CONFLICT DO UPDATE SET Offset = excluded.Offset;
            """
            Parameters = [ "@ChartId", SqliteType.Text, -1; "@Offset", SqliteType.Real, 4 ]
            FillParameters =
                fun p (chart_id, offset) ->
                    p.String chart_id
                    p.Float32(float32 offset)
        }

    let save_offsets (changes: (string * Time) seq) (db: Database) =
        SAVE_OFFSET.Batch changes db |> expect |> ignore

    let private SAVE_LAST_PLAYED: NonQuery<string * int64> =
        {
            SQL =
                """
                INSERT INTO chart_data (ChartId, Offset, LastPlayed, Comment, Breakpoints, PersonalBests)
                VALUES (@ChartId, 0, @LastPlayed, '', '[]', '{}')
                ON CONFLICT DO UPDATE SET LastPlayed = excluded.LastPlayed;
            """
            Parameters = [ "@ChartId", SqliteType.Text, -1; "@LastPlayed", SqliteType.Integer, 8 ]
            FillParameters =
                fun p (chart_id, last_played) ->
                    p.String chart_id
                    p.Int64 last_played
        }

    let save_last_played (changes: (string * int64) seq) (db: Database) =
        SAVE_LAST_PLAYED.Batch changes db |> expect |> ignore

    let private SAVE_COMMENT: NonQuery<string * string> =
        {
            SQL =
                """
                INSERT INTO chart_data (ChartId, Offset, LastPlayed, Comment, Breakpoints, PersonalBests)
                VALUES (@ChartId, 0, 0, @Comment, '[]', '{}')
                ON CONFLICT DO UPDATE SET Comment = excluded.Comment;
            """
            Parameters = [ "@ChartId", SqliteType.Text, -1; "@Comment", SqliteType.Text, -1 ]
            FillParameters =
                fun p (chart_id, comment) ->
                    p.String chart_id
                    p.String comment
        }

    let save_comments (changes: (string * string) seq) (db: Database) =
        SAVE_COMMENT.Batch changes db |> expect |> ignore

    let private SAVE_BREAKPOINTS: NonQuery<string * Time list> =
        {
            SQL =
                """
                INSERT INTO chart_data (ChartId, Offset, LastPlayed, Comment, Breakpoints, PersonalBests)
                VALUES (@ChartId, 0, 0, '', @Breakpoints, '{}')
                ON CONFLICT DO UPDATE SET Breakpoints = excluded.Breakpoints;
            """
            Parameters = [ "@ChartId", SqliteType.Text, -1; "@Breakpoints", SqliteType.Text, -1 ]
            FillParameters =
                fun p (chart_id, breakpoints) ->
                    p.String chart_id
                    p.Json JSON breakpoints
        }

    let save_breakpoints (changes: (string * Time list) seq) (db: Database) =
        SAVE_BREAKPOINTS.Batch changes db |> expect |> ignore

    let private SAVE_PERSONAL_BESTS: NonQuery<string * Map<string, Bests>> =
        {
            SQL =
                """
                INSERT INTO chart_data (ChartId, Offset, LastPlayed, Comment, Breakpoints, PersonalBests)
                VALUES (@ChartId, 0, 0, '', '[]', json(@PersonalBests))
                ON CONFLICT DO UPDATE SET PersonalBests = excluded.PersonalBests;
            """
            Parameters = [ "@ChartId", SqliteType.Text, -1; "@PersonalBests", SqliteType.Text, -1 ]
            FillParameters =
                fun p (chart_id, bests) ->
                    p.String chart_id
                    p.Json JSON bests
        }

    let save_personal_bests (changes: (string * Map<string, Bests>) seq) (db: Database) =
        SAVE_PERSONAL_BESTS.Batch changes db |> expect |> ignore

    let private FAST_LOAD: Query<int64, string * DbChartData> =
        {
            SQL =
                """
            SELECT ChartId, Offset, LastPlayed, Comment, Breakpoints, PersonalBests FROM chart_data
            LIMIT 1000
            OFFSET @Offset;
            """
            Parameters = [ "@Offset", SqliteType.Integer, 8 ]
            FillParameters = fun p page -> p.Int64(page * 1000L)
            Read =
                (fun r ->
                    r.String,
                    {
                        Offset = r.Float32 |> Time.of_number
                        LastPlayed = r.Int64
                        Comment = r.String
                        Breakpoints = r.Json JSON
                        PersonalBests = r.Json JSON
                    }
                )
        }

    let fast_load (db: Database) : (string * DbChartData) seq =
        seq {
            let mutable batch = 0L
            let mutable next_batch = FAST_LOAD.Execute batch db |> expect
            yield! next_batch

            while next_batch.Length = 1000 do
                batch <- batch + 1L
                next_batch <- FAST_LOAD.Execute batch db |> expect
                yield! next_batch
        }