namespace Prelude.Data.Library

open System.IO
open Percyqaz.Common
open Percyqaz.Data.Sqlite
open Prelude
open Prelude.Charts
open Prelude.Calculator.Patterns

module DbCharts =

    let private no_nan x = if System.Single.IsFinite x then x else 0.0f

    // Increment this to recalculate pattern & rating data
    let private CALC_VERSION = 5uy

    // todo: create binary representation of Patterns data for much faster reading/writing and storage efficiency
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
                Packs TEXT NOT NULL,
                Origin TEXT NOT NULL,
                Keys INTEGER NOT NULL,
                Length REAL NOT NULL,
                BPM INTEGER NOT NULL,
                DateAdded INTEGER NOT NULL,
                CalcVersion INTEGER NOT NULL,
                Rating REAL NOT NULL,
                Patterns TEXT NOT NULL,
                Chart BLOB NOT NULL
            );
            """
        }

    let internal RESET_OSU_QUAVER_ORIGINS: NonQuery<unit> =
        { NonQuery.without_parameters() with
            SQL =
                """
                UPDATE charts
                SET Origin =
                (
	                SELECT coalesce('[' || group_concat(json_each.value) || ']', '[]')
	                FROM json_each(charts.Origin)
	                WHERE json_each.value NOT LIKE '%Osu%'
	                AND json_each.value NOT LIKE '%Quaver%'
                );
            """
        }

    let internal RESET_ORIGINS: NonQuery<unit> =
        { NonQuery.without_parameters() with
            SQL = """ UPDATE charts SET Origin = '[]'; """
        }

    let private SAVE: NonQuery<ChartMeta * Chart> =
        {
            SQL =
                """
                INSERT INTO charts (
                    Id,
                    Title, TitleNative, Artist, ArtistNative,
                    DifficultyName, Subtitle, Source, Creator, Tags,
                    Background, Audio, PreviewTime,
                    Packs, Origin,
                    Keys, Length, BPM,
                    DateAdded, CalcVersion, Rating, Patterns,
                    Chart
                )
                VALUES (
                    @Id,
                    @Title, @TitleNative, @Artist, @ArtistNative,
                    @DifficultyName, @Subtitle, @Source, @Creator, @Tags,
                    json(@Background), json(@Audio), @PreviewTime,
                    json(@Packs), json(@Origins),
                    @Keys, @Length, @BPM,
                    @DateAdded, @CalcVersion, @Rating, json(@Patterns),
                    @Chart)
                ON CONFLICT DO UPDATE SET
                    Title = excluded.Title,
                    TitleNative = excluded.TitleNative,
                    Artist = excluded.Artist,
                    ArtistNative = excluded.ArtistNative,
                    DifficultyName = excluded.DifficultyName,
                    Subtitle = excluded.Subtitle,
                    Source = excluded.Source,
                    Creator = excluded.Creator,
                    Tags = excluded.Tags,
                    Background = excluded.Background,
                    Audio = excluded.Audio,
                    PreviewTime = excluded.PreviewTime,
                    DateAdded = excluded.DateAdded,
                    Packs = (
                        SELECT json_group_array(value) FROM (
                            SELECT json_each.value
                            FROM charts, json_each(charts.Packs)
                            WHERE charts.Id = excluded.Id
                            UNION
                            SELECT json_each.value
                            FROM json_each(excluded.Packs)
                        )
                    ),
                    Origin = (
                        SELECT coalesce('[' || group_concat(value) || ']', '[]') FROM (
                            SELECT json_each.value
                            FROM charts, json_each(charts.Origin)
                            WHERE charts.Id = excluded.Id
                            UNION
                            SELECT json_each.value
                            FROM json_each(excluded.Origin)
                        )
                    ),
                    Length = excluded.Length,
                    BPM = excluded.BPM,
                    DateAdded = excluded.DateAdded,
                    CalcVersion = excluded.CalcVersion,
                    Rating = excluded.Rating,
                    Patterns = excluded.Patterns,
                    Chart = excluded.Chart;
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
                    "@Packs", SqliteType.Text, -1
                    "@Origins", SqliteType.Text, -1
                    "@Keys", SqliteType.Integer, 1
                    "@Length", SqliteType.Real, 4
                    "@BPM", SqliteType.Integer, 4
                    "@DateAdded", SqliteType.Integer, 8
                    "@CalcVersion", SqliteType.Integer, 1
                    "@Rating", SqliteType.Real, 4
                    "@Patterns", SqliteType.Text, -1
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
                    p.Float32 (float32 db_chart.PreviewTime |> no_nan)
                    p.Json JSON db_chart.Packs
                    p.Json JSON db_chart.Origins
                    p.Byte (byte db_chart.Keys)
                    p.Float32 (float32 db_chart.Length |> no_nan)
                    p.Int32 db_chart.BPM
                    p.Int64 db_chart.DateAdded
                    p.Byte CALC_VERSION
                    p.Float32 (no_nan db_chart.Rating)
                    p.Json JSON db_chart.Patterns
                    p.Blob (
                        use ms = new MemoryStream()
                        use bw = new BinaryWriter(ms)
                        Chart.write_headless chart_data bw
                        ms.ToArray()
                   )
                )
        }

    let save (chart_meta: ChartMeta) (chart: Chart) (db: Database) : unit =
        SAVE.Execute (chart_meta, chart) db |> expect |> ignore

    let save_batch (charts: (ChartMeta * Chart) seq) (db: Database) : unit =
        SAVE.Batch charts db |> expect |> ignore

    let private read_meta (r: RowReaderHelper) : ChartMeta =
        let mutable calc_version = 0uy
        {
            Hash = r.String
            Title = r.String
            TitleNative = r.StringOption
            Artist = r.String
            ArtistNative = r.StringOption
            DifficultyName = r.String
            Subtitle = r.StringOption
            Source = r.StringOption
            Creator = r.String
            Tags = r.Json JSON
            Background = r.Json JSON
            Audio = r.Json JSON
            PreviewTime = r.Float32 |> Time.of_number
            Packs = r.Json JSON
            Origins = r.Json JSON
            Keys = r.Byte |> int32
            Length = r.Float32 |> Time.of_number
            BPM = r.Int32
            DateAdded = r.Int64
            Rating =
                calc_version <- r.Byte
                r.Float32
            Patterns =
                if calc_version <> CALC_VERSION then
                    r.String |> ignore
                    PatternReport.Default
                else r.Json JSON
        }

    let private GET_META: Query<string, ChartMeta> =
        {
            SQL =
                """
            SELECT
                Id,
                Title, TitleNative, Artist, ArtistNative,
                DifficultyName, Subtitle, Source, Creator, Tags,
                Background, Audio, PreviewTime,
                Packs, Origin,
                Keys, Length, BPM,
                DateAdded, CalcVersion, Rating, Patterns
            FROM charts
            WHERE Id = @Hash;
            """
            Parameters = [ "@Hash", SqliteType.Text, -1 ]
            FillParameters = fun p hash -> p.String hash
            Read = read_meta
        }

    let get_meta (hash: string) (db: Database) : ChartMeta option =
        GET_META.Execute hash db |> expect |> Array.tryExactlyOne

    let private FAST_LOAD: Query<int, ChartMeta> =
        {
            SQL =
                """
            SELECT
                Id,
                Title, TitleNative, Artist, ArtistNative,
                DifficultyName, Subtitle, Source, Creator, Tags,
                Background, Audio, PreviewTime,
                Packs, Origin,
                Keys, Length, BPM,
                DateAdded, CalcVersion, Rating, Patterns
            FROM charts
            LIMIT 4000
            OFFSET @Offset;
            """
            Parameters = [ "@Offset", SqliteType.Integer, 4 ]
            FillParameters = fun p page -> p.Int32(page * 4000)
            Read = read_meta
        }

    let fast_load (db: Database) : ChartMeta seq =
        seq {
            let mutable batch = 0
            let mutable next_batch = FAST_LOAD.Execute batch db |> expect
            yield! next_batch

            while next_batch.Length = 4000 do
                batch <- batch + 1
                next_batch <- FAST_LOAD.Execute batch db |> expect
                yield! next_batch
        }

    let private GET_CHART: Query<string, Result<Chart, string>> =
        {
            SQL = """SELECT Keys, Chart FROM charts WHERE Id = @Hash;"""
            Parameters = [ "@Hash", SqliteType.Text, -1 ]
            FillParameters = fun p hash -> p.String hash
            Read = fun r ->
                let keys = r.Byte |> int32
                use stream = r.Stream
                use br = new BinaryReader(stream)
                Chart.read_headless keys br
        }

    let get_chart (hash: string) (db: Database) : Result<Chart, string> =
        GET_CHART.Execute hash db
        |> expect
        |> Array.tryExactlyOne
        |> function None -> Error "No chart in the database matching this hash" | Some res -> res

    let private DELETE: NonQuery<string> =
        {
            SQL = """DELETE FROM charts WHERE Id = @Hash;"""
            Parameters = [ "@Hash", SqliteType.Text, -1 ]
            FillParameters = fun p hash -> p.String hash
        }

    let delete (hash: string) (db: Database) : bool =
        DELETE.Execute hash db |> expect > 0

    let delete_batch (hashes: string seq) (db: Database) : int =
        DELETE.Batch hashes db |> expect

    let private UPDATE_CALCULATED_DATA: NonQuery<string * float32 * PatternReport> =
        {
            SQL = """
            UPDATE charts
            SET
                CalcVersion = @CalcVersion,
                Rating = @Rating,
                Patterns = json(@Patterns)
            WHERE Id = @Hash;
            """
            Parameters = [
                "@Hash", SqliteType.Text, -1
                "@CalcVersion", SqliteType.Integer, 1
                "@Rating", SqliteType.Real, 4
                "@Patterns", SqliteType.Text, -1
            ]
            FillParameters =
                (fun p (hash, rating, patterns) ->
                    p.String hash
                    p.Byte CALC_VERSION
                    p.Float32 rating
                    p.Json JSON patterns
                )
        }

    let update_calculated_data (chunk: (string * float32 * PatternReport) seq) (db: Database) =
        UPDATE_CALCULATED_DATA.Batch chunk db |> expect |> ignore

    let private UPDATE_PACKS: NonQuery<string * Set<string>> =
        {
            SQL = """
            UPDATE charts
            SET
                Packs = json(@Packs)
            WHERE Id = @Hash;
            """
            Parameters = [
                "@Hash", SqliteType.Text, -1
                "@Packs", SqliteType.Text, -1
            ]
            FillParameters =
                (fun p (hash, packs) ->
                    p.String hash
                    p.Json JSON packs
                )
        }

    let update_packs_batch (chunk: (string * Set<string>) seq) (db: Database) =
        UPDATE_PACKS.Batch chunk db |> expect |> ignore