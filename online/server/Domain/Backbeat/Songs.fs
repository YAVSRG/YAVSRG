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

    let add_chart_song (chart_id: string) (chart: Chart) (song: Song) = failwith "nyi"
    let add_chart (chart_id: string) (chart: Chart) (song_id: int64) = failwith "nyi"
    let merge_songs (song_a_id: int64) (song_b_id: int64) = 
        //transact
        // point all charts with song_a_id to song_b_id
        // delete song_a
        //transact
        failwith "nyi"
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