namespace Interlude.Web.Server.Domain.Backbeat

open Prelude
open Percyqaz.Common
open Percyqaz.Data
open Percyqaz.Data.Sqlite
open Prelude.Common
open Interlude.Web.Server

// modelled after Prelude.Backbeat.Archive.Song
// todo: modify those types to exactly match these and then share them
type Song =
    {
        Artists: string list
        OtherArtists: string list
        Remixers: string list
        Title: string
        AlternativeTitles: string list
        Source: string option
        Tags: string list
    }

[<Json.AutoCodec>]
type ChartSource =
    | Osu of {| BeatmapId: int; BeatmapSetId: int |}
    | Stepmania of id: int
    | CommunityPack of id: string

type Chart =
    {
        Creators: string list
        DifficultyName: string
        Subtitle: string option
        Tags: string list
        Duration: Time
        PreviewTime: Time
        Notecount: int
        Keys: int
        BPM: (float32<ms / beat> * float32<ms / beat>)
        BackgroundHash: string
        AudioHash: string
        Sources: ChartSource list
    }

module Songs =
    
    let internal CREATE_TABLES : NonQuery<unit> =
        { NonQuery.without_parameters() with
            SQL = """
            BEGIN TRANSACTION;

            CREATE TABLE songs (
                Id INTEGER PRIMARY KEY NOT NULL,
                Artists TEXT NOT NULL,
                OtherArtists TEXT NOT NULL,
                Remixers TEXT NOT NULL,
                Title TEXT NOT NULL,
                AlternativeTitles TEXT NOT NULL,
                Source TEXT NOT NULL,
                Tags TEXT NOT NULL
            );
            
            CREATE TABLE charts (
                Id TEXT PRIMARY KEY NOT NULL,
                SongId INTEGER NOT NULL,
                Creators TEXT NOT NULL,
                DifficultyName TEXT NOT NULL,
                Subtitle TEXT,
                Tags TEXT NOT NULL,
                Duration REAL NOT NULL,
                PreviewTime REAL NOT NULL,
                Notecount INTEGER NOT NULL,
                Keys INTEGER NOT NULL,
                BPM TEXT NOT NULL,
                BackgroundHash TEXT NOT NULL,
                AudioHash TEXT NOT NULL,
                Sources TEXT NOT NULL,
                FOREIGN KEY (SongId) REFERENCES songs(Id) ON DELETE RESTRICT,
            );

            COMMIT;
            """
        }

    let private ADD_CHART_SONG : Query<string * Chart * Song, int64> = 
        {
            SQL = """
            BEGIN TRANSACTION;

            INSERT INTO songs (Artists, OtherArtists, Remixers, Title, AlternativeTitles, Source, Tags)
            VALUES (@Artists, @OtherArtists, @Remixers, @Title, @AlternativeTitles, @Source, @Tags);

            INSERT INTO charts (Id, SongId, Creators, DifficultyName, Subtitle, Tags, Duration, PreviewTime, Notecount, Keys, BPM, BackgroundHash, AudioHash, Sources)
            VALUES (@ChartId, last_insert_rowid(), @Creators, @DifficultyName, @Subtitle, @ChartTags, @Duration, @PreviewTime, @Notecount, @Keys, @BPM, @BackgroundHash, @AudioHash, @Sources)
            RETURNING SongId;
            
            COMMIT;
            """
            Parameters = [
                "@Artists", SqliteType.Text, -1
                "@OtherArtists", SqliteType.Text, -1
                "@Remixers", SqliteType.Text, -1
                "@Title", SqliteType.Text, -1
                "@AlternativeTitles", SqliteType.Text, -1
                "@Source", SqliteType.Text, -1
                "@Tags", SqliteType.Text, -1
                "@ChartId", SqliteType.Text, -1
                "@Creators", SqliteType.Text, -1
                "@DifficultyName", SqliteType.Text, -1
                "@Subtitle", SqliteType.Text, -1
                "@ChartTags", SqliteType.Text, -1
                "@Duration", SqliteType.Real, 4
                "@PreviewTime", SqliteType.Real, 4
                "@Notecount", SqliteType.Integer, 4
                "@Keys", SqliteType.Integer, 1
                "@BPM", SqliteType.Text, -1
                "@BackgroundHash", SqliteType.Text, -1
                "@AudioHash", SqliteType.Text, -1
                "@Sources", SqliteType.Text, -1
            ]
            FillParameters = (fun p (chart_id, chart, song) ->
                p.Json JSON song.Artists
                p.Json JSON song.OtherArtists
                p.Json JSON song.Remixers
                p.String song.Title
                p.Json JSON song.AlternativeTitles
                p.StringOption song.Source
                p.Json JSON song.Tags
                p.String chart_id
                p.Json JSON chart.Creators
                p.String chart.DifficultyName
                p.StringOption chart.Subtitle
                p.Json JSON chart.Tags
                p.Float32 (float32 chart.Duration)
                p.Float32 (float32 chart.PreviewTime)
                p.Int32 chart.Notecount
                p.Byte (uint8 chart.Keys)
                p.Json JSON chart.BPM
                p.String chart.BackgroundHash
                p.String chart.AudioHash
                p.Json JSON chart.Sources
            )
            Read = fun r -> r.Int64
        }
    let add_chart_song (chart_id: string) (chart: Chart) (song: Song) : int64 = ADD_CHART_SONG.Execute (chart_id, chart, song) backbeat_db |> expect |> Array.exactlyOne

    let private ADD_CHART : NonQuery<string * Chart * int64> = 
        {
            SQL = """
            INSERT INTO charts (Id, SongId, Creators, DifficultyName, Subtitle, Tags, Duration, PreviewTime, Notecount, Keys, BPM, BackgroundHash, AudioHash, Sources)
            VALUES (@ChartId, @SongId, @Creators, @DifficultyName, @Subtitle, @ChartTags, @Duration, @PreviewTime, @Notecount, @Keys, @BPM, @BackgroundHash, @AudioHash, @Sources);
            """
            Parameters = [
                "@ChartId", SqliteType.Text, -1
                "@SongId", SqliteType.Integer, 8
                "@Creators", SqliteType.Text, -1
                "@DifficultyName", SqliteType.Text, -1
                "@Subtitle", SqliteType.Text, -1
                "@ChartTags", SqliteType.Text, -1
                "@Duration", SqliteType.Real, 4
                "@PreviewTime", SqliteType.Real, 4
                "@Notecount", SqliteType.Integer, 4
                "@Keys", SqliteType.Integer, 1
                "@BPM", SqliteType.Text, -1
                "@BackgroundHash", SqliteType.Text, -1
                "@AudioHash", SqliteType.Text, -1
                "@Sources", SqliteType.Text, -1
            ]
            FillParameters = (fun p (chart_id, chart, song_id) ->
                p.String chart_id
                p.Int64 song_id
                p.Json JSON chart.Creators
                p.String chart.DifficultyName
                p.StringOption chart.Subtitle
                p.Json JSON chart.Tags
                p.Float32 (float32 chart.Duration)
                p.Float32 (float32 chart.PreviewTime)
                p.Int32 chart.Notecount
                p.Byte (uint8 chart.Keys)
                p.Json JSON chart.BPM
                p.String chart.BackgroundHash
                p.String chart.AudioHash
                p.Json JSON chart.Sources
            )
        }
    let add_chart (chart_id: string) (chart: Chart) (song_id: int64) = ADD_CHART.Execute (chart_id, chart, song_id) backbeat_db |> expect |> ignore

    let private MERGE_SONGS : NonQuery<int64 * int64> =
        {
            SQL = """
            BEGIN TRANSACTION;

            UPDATE charts
            SET SongId = @NewId
            WHERE SongId = @OldId;

            DELETE FROM songs
            WHERE SongId = @OldId;

            COMMIT;
            """
            Parameters = [ "@NewId", SqliteType.Integer, 8; "@OldId", SqliteType.Integer, 8 ]
            FillParameters = fun p (old_id, new_id) -> p.Int64 old_id; p.Int64 new_id
        }
    let merge_songs (old_song_id: int64) (new_song_id: int64) = MERGE_SONGS.Execute (old_song_id, new_song_id) backbeat_db |> expect |> ignore

    let update_chart (chart_id: string) (chart: Chart) = failwith "nyi"
    let update_song (song_id: int64) (song: Song) = failwith "nyi"
    let delete_chart (chart_id: string) =
        //transact
        // delete chart
        // attempt delete song matching id or ignore
        //transact
        failwith "nyi"
    let get_chart (chart_id: string) : (Chart * Song) option = failwith "nyi"
    let get_song (song_id: int64) : Song option = failwith "nyi"

    // bunch of search methods