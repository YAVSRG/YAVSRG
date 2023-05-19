namespace Interlude.Web.Bot

open System.IO
open System.Diagnostics
open System.Collections.Generic
open Percyqaz.Common
open Prelude
open Prelude.Data.Charts.Archive

module Charts =

    let mutable songs = Songs()

    let mutable charts = Charts()

    let init() =
        if not (Directory.Exists "Backbeat") then
            Process.Start(ProcessStartInfo("git", "clone --depth 1 https://github.com/YAVSRG/Backbeat.git")).WaitForExit()
        let ARCHIVE_PATH = Path.Combine("Backbeat", "archive")
        charts <- 
            match JSON.FromFile (Path.Combine(ARCHIVE_PATH, "charts.json")) with
            | Ok d -> d
            | Error e -> 
                Logging.Warn(sprintf "Error loading charts.json: %s" e.Message)
                Dictionary<ChartHash, Chart>()
        songs <-
            match JSON.FromFile (Path.Combine(ARCHIVE_PATH, "songs.json")) with
            | Ok d -> d
            | Error e -> 
                Logging.Warn(sprintf "Error loading songs.json: %s" e.Message)
                Dictionary<SongId, Song>()

    let search_for_songs(query: string) =
        let query = query.ToLower()
        seq {
            for song_id in songs.Keys do
                let song = songs.[song_id]
                if song.FormattedTitle.ToLower().Contains(query) then
                    yield (song_id, song)
        } |> Seq.truncate 20
            