namespace Prelude.Data.Charts

open System.IO
open Percyqaz.Common
open Percyqaz.Data
open Percyqaz.Data.Sqlite
open Prelude
open Prelude.Charts
open Prelude.Charts.Processing.Difficulty
open Prelude.Charts.Processing.Patterns

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
    static member FromChart (file_timestamp: int64) (chart: Chart) =
        let source_folder_path = Path.GetDirectoryName(chart.LoadedFromPath)
        let source_folder_name = Path.GetDirectoryName(chart.LoadedFromPath) |> Path.GetFileName
        {
            Hash = Chart.hash chart

            Title = chart.Header.Title
            TitleNative = chart.Header.TitleNative
            Artist = chart.Header.Artist
            ArtistNative = chart.Header.ArtistNative
            DifficultyName = chart.Header.DiffName
            Subtitle = chart.Header.Subtitle
            Source = chart.Header.Source
            Creator = chart.Header.Creator
            Tags = chart.Header.Tags

            Background =
                match chart.Header.BackgroundFile with
                | MediaPath.Asset s -> DbAssetPath.Hash s
                | MediaPath.Relative f -> DbAssetPath.Absolute (Path.Combine(source_folder_path, f))
                | MediaPath.Absolute p -> DbAssetPath.Absolute p
                | MediaPath.Missing -> DbAssetPath.Missing
            Audio =
                match chart.Header.AudioFile with
                | MediaPath.Asset s -> DbAssetPath.Hash s
                | MediaPath.Relative f -> DbAssetPath.Absolute (Path.Combine(source_folder_path, f))
                | MediaPath.Absolute p -> DbAssetPath.Absolute p
                | MediaPath.Missing -> DbAssetPath.Missing
            PreviewTime = chart.Header.PreviewTime

            Folder = source_folder_name
            Origin = 
                match chart.Header.ChartSource with
                | Origin.Osu (set, map) -> DbChartOrigin.Osu(set, map)
                | Origin.Quaver (set, map) -> DbChartOrigin.Quaver(set, map)
                | Origin.Etterna pack -> DbChartOrigin.Etterna pack
                | Origin.Stepmania _ -> DbChartOrigin.Etterna source_folder_name
                | Origin.Unknown -> DbChartOrigin.Unknown

            Keys = chart.Keys
            Length = chart.LastNote - chart.FirstNote
            BPM = 
                let mspb = Chart.find_most_common_bpm chart
                let bpm = 60000.0f<ms/minute> / mspb |> float32
                if System.Single.IsFinite(bpm) then 
                    bpm |> round |> int 
                else 0
            DateAdded = file_timestamp
            Rating = (DifficultyRating.calculate 1.0f chart.Notes).Physical |> float32
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
                Folder TEXT NOT NULL,
                Origin TEXT NOT NULL,
                Keys INTEGER NOT NULL,
                Length REAL NOT NULL,
                BPM INTEGER NOT NULL,
                DateAdded INTEGER NOT NULL,
                Rating REAL NOT NULL,
                Chart BLOB NOT NULL
            );
            """
        }

    let private SAVE: NonQuery<DbChart * Chart> =
        {
            SQL =
                """
                INSERT OR REPLACE INTO charts (Id, Title, TitleNative, Artist, ArtistNative, DifficultyName, Subtitle, Source, Creator, Tags, Background, Audio, PreviewTime, Folder, Origin, Keys, Length, BPM, DateAdded, Rating, Chart)
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
        