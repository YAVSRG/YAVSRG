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

    let mutable packs = { Stepmania = new Dictionary<StepmaniaPackId, StepmaniaPack>(); Community = new Dictionary<CommunityPackId, CommunityPack>() }

    let init() =
        if not (Directory.Exists "Backbeat") then
            Process.Start(ProcessStartInfo("git", "clone --depth 1 https://github.com/YAVSRG/Backbeat.git")).WaitForExit()
        else Process.Start(ProcessStartInfo("git", "pull -f", WorkingDirectory = "Backbeat")).WaitForExit()
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
        packs <-
            match JSON.FromFile (Path.Combine(ARCHIVE_PATH, "packs.json")) with
            | Ok d -> d
            | Error e -> 
                Logging.Warn(sprintf "Error loading packs.json: %s" e.Message)
                {
                    Stepmania = Dictionary<StepmaniaPackId, StepmaniaPack>()
                    Community = Dictionary<CommunityPackId, CommunityPack>()
                }

    let search_for_charts(query: string) =
        let query = query.ToLower()
        seq {
            for chart_hash in charts.Keys do
                let chart = charts.[chart_hash]
                let song = songs.[chart.SongId]
                if song.FormattedTitle.ToLower().Contains(query) then
                    yield (chart, song)
        } 
        |> Seq.truncate 30
        |> Seq.groupBy snd
        |> Seq.map (fun (song, stuff) -> song, Seq.map fst stuff |> List.ofSeq)

    let format_source (source: ChartSource) =
        match source with
        | Osu data -> sprintf "[osu! Beatmap](https://osu.ppy.sh/beatmapsets/%i#mania/%i)" data.BeatmapSetId data.BeatmapId
        | Stepmania id -> sprintf "[%s](%s)" packs.Stepmania.[id].Title (DownloadUrl.unpickle packs.Stepmania.[id].Mirrors.Head)
        | CommunityPack data -> sprintf "[%s](%s)" packs.Community.[data.PackId].Title (DownloadUrl.unpickle packs.Community.[data.PackId].Mirrors.Head)

