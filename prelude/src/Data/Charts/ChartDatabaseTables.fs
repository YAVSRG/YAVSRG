namespace Prelude.Data.Charts

open Percyqaz.Common
open Percyqaz.Data
open Percyqaz.Data.Sqlite
open Prelude
open Prelude.Charts

[<Json.AutoCodec>]
type DbAssetPath =
    | Absolute of string
    | Hash of string
    | Missing

[<Json.AutoCodec>]
type DbChartOrigin =
    | Osu of beatmapsetid: int * beatmapid: int
    | Quaver of mapsetid: int * mapid: int
    | Etterna of pack_name: string
    | Unknown

type DbChart =
    {
        Hash: string

        Title: string
        TitleNative: string option
        Artist: string
        ArtistNative: string option
        DifficultyName: string
        Subtitle: string option
        Source: string option
        Creator: string
        Tags: string list

        Background: DbAssetPath
        Audio: DbAssetPath
        PreviewTime: Time

        Folder: string
        Origin: DbChartOrigin

        Keys: int
        Length: Time
        BPM: int
        DateAdded: int64
        Rating: float32
    }

module DbCharts =

    let internal CREATE_TABLE: NonQuery<unit> =
        { NonQuery.without_parameters () with
            SQL =
                """
            CREATE TABLE charts (
                Id TEXT PRIMARY KEY NOT NULL,
                Title TEXT NOT NULL,
                TitleNative TEXT,
                Artist TEXT NOT NULL,
                ArtistNative TEXT,
                DifficultyName TEXT NOT NULL,
                Subtitle TEXT,
                Source TEXT,
                Creator TEXT NOT NULL,
                Tags TEXT NOT NULL,
                Background TEXT NOT NULL,
                Audio TEXT NOT NULL,
                PreviewTime REAL NOT NULL,
                Keys INTEGER NOT NULL,
                Length REAL NOT NULL,
                BPM INTEGER NOT NULL,
                DateAdded INTEGER NOT NULL,
                Rating REAL NOT NULL,
                Folder TEXT NOT NULL,
                Chart BLOB NOT NULL
            );
            """
        }

    let private SAVE: NonQuery<DbChart * Chart> =
        {
            SQL =
                """
                INSERT INTO songs (Id, Title, TitleNative, Artist, ArtistNative, DifficultyName, Subtitle, Source, Creator, Tags, Background, Audio, PreviewTime, Folder, Origin, Keys, Length, BPM, DateAdded, Rating, Chart)
                VALUES (@Id, @Title, @TitleNative, @Artist, @ArtistNative, @DifficultyName, @Subtitle, @Source, @Creator, @Tags, @Background, @Audio, @PreviewTime, @Folder, @Origin, @Keys, @Length, @BPM, @DateAdded, @Rating, @Chart);
            """
            Parameters =
                [
                    "@Id", SqliteType.Text, -1
                    "@Title", SqliteType.Text, -1
                    "@TitleNative", SqliteType.Text, -1
                    "@Artist", SqliteType.Text, -1
                    "@ArtistNative", SqliteType.Text, -1
                    "@DifficultyName", SqliteType.Text, -1
                    "@Subtitle", SqliteType.Text, -1
                    "@Source", SqliteType.Text, -1
                    "@Creator", SqliteType.Text, -1
                    "@Tags", SqliteType.Text, -1
                    "@Background", SqliteType.Text, -1
                    "@Audio", SqliteType.Text, -1
                    "@PreviewTime", SqliteType.Real, 4
                    "@Folder", SqliteType.Text, -1
                    "@Origin", SqliteType.Text, -1
                    "@Keys", SqliteType.Integer, 1
                    "@Length", SqliteType.Real, 4
                    "@BPM", SqliteType.Integer, 4
                    "@DateAdded", SqliteType.Integer, 8
                    "@Rating", SqliteType.Real, 8
                    "@Chart", SqliteType.Blob, -1
                ]
            FillParameters =
                (fun p (db_chart, chart_data) ->
                    p.String db_chart.Hash
                    p.String db_chart.Title
                    p.StringOption db_chart.TitleNative
                    p.String db_chart.Artist
                    p.StringOption db_chart.ArtistNative
                    p.String db_chart.DifficultyName
                    p.StringOption db_chart.Subtitle
                    p.StringOption db_chart.Source
                    p.String db_chart.Creator
                    p.Json JSON db_chart.Tags
                    p.Json JSON db_chart.Background
                    p.Json JSON db_chart.Audio
                    p.Float32 (float32 db_chart.PreviewTime)
                    p.String db_chart.Folder
                    p.Json JSON db_chart.Origin
                    p.Byte (byte db_chart.Keys)
                    p.Float32 (float32 db_chart.Length)
                    p.Int32 db_chart.BPM
                    p.Int64 db_chart.DateAdded
                    p.Float32 db_chart.Rating
                    p.Blob (
                        use ms = new System.IO.MemoryStream()
                        use bw = new System.IO.BinaryWriter(ms)
                        Chart.write_headless chart_data bw
                        ms.ToArray()
                   )
                )
        }

    let save (db_chart: DbChart) (chart: Chart) (db: Database) : unit =
        SAVE.Execute (db_chart, chart) db |> expect |> ignore

    let save_batch (charts: (DbChart * Chart) seq) (db: Database) : unit =
        SAVE.Batch charts db |> expect |> ignore
        