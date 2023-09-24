namespace Interlude.Features.Import

open System
open System.Text
open System.IO
open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude.Charts.Formats.``osu!``
open Prelude.Charts.Formats.Interlude
open Prelude.Charts.Formats.Conversions
open Prelude.Gameplay
open Prelude.Data.``osu!``
open Prelude.Data.Scores
open Prelude.Data.Charts
open Prelude.Data.Charts.Caching
open Prelude.Data.Charts.Library.Imports
open Interlude.Options

module Scores =

    let private import_osu_scores() =
        match options.OsuMount.Value with
        | None -> Logging.Warn "Requires osu! Songs folder to be mounted"
        | Some m ->

        let scores =
            use file = Path.Combine(m.SourceFolder, "..", "scores.db") |> File.OpenRead
            Logging.Info "Reading scores database .."
            use reader = new BinaryReader(file, Encoding.UTF8)
            ScoreDatabase.Read(reader)

        Logging.Info (sprintf "Read score data, containing info about %i maps" scores.Beatmaps.Length)
        
        let main_db =
            use file = Path.Combine(m.SourceFolder, "..", "osu!.db") |> File.OpenRead
            Logging.Info "Reading osu! database .."
            use reader = new BinaryReader(file, Encoding.UTF8)
            OsuDatabase.Read(reader)

        Logging.Info (sprintf "Read %s's osu! database containing %i maps, starting import .." main_db.PlayerName main_db.Beatmaps.Length)

        let mutable chart_count = 0
        let mutable score_count = 0

        let find_matching_chart (beatmap_data: OsuDatabase_Beatmap) (chart: Chart) =
            let chart_hash = Chart.hash chart
            match Cache.by_hash chart_hash Library.cache with
            | None -> 
                match ``osu!``.detect_rate_mod beatmap_data.Difficulty with
                | Some rate ->
                    let chart = Chart.scale rate chart
                    let chart_hash = Chart.hash chart
                    match Cache.by_hash chart_hash Library.cache with
                    | None -> 
                        Logging.Warn(sprintf "Skipping %.2fx of %s [%s], can't find a matching 1.00x chart" rate beatmap_data.TitleUnicode beatmap_data.Difficulty)
                        None
                    | Some _ ->
                        Some (chart, chart_hash, rate)
                | None -> 
                    Logging.Warn(sprintf "%s [%s] skipped" beatmap_data.TitleUnicode beatmap_data.Difficulty)
                    None
            | Some _ -> Some (chart, chart_hash, 1.0f)

        for beatmap_score_data in scores.Beatmaps |> Seq.where (fun b -> b.Scores.Length > 0 && b.Scores.[0].Mode = 3uy) do
            match main_db.Beatmaps |> Seq.tryFind (fun b -> b.Hash = beatmap_score_data.Hash && b.Mode = 3uy) with
            | None -> ()
            | Some beatmap_data ->

            let osu_file = Path.Combine(m.SourceFolder, beatmap_data.FolderName, beatmap_data.Filename)
            match
                try
                    loadBeatmapFile osu_file
                    |> fun b -> ``osu!``.toInterlude b { Config = ConversionOptions.Default; Source = osu_file }
                    |> Some
                with _ -> None
            with
            | None -> ()
            | Some chart ->

            match find_matching_chart beatmap_data chart with
            | None -> ()
            | Some (chart, _, rate) ->

            chart_count <- chart_count + 1

            for score in beatmap_score_data.Scores do
                let replay_file = Path.Combine(m.SourceFolder, "..", "Data", "r", sprintf "%s-%i.osr" score.BeatmapHash score.Timestamp)
                match
                    try
                        use file = File.OpenRead replay_file
                        use br = new BinaryReader(file)
                        Some (ScoreDatabase_Score.Read br)
                    with err -> Logging.Error(sprintf "Error loading replay file %s" replay_file, err); None
                with
                | None -> ()
                | Some replay_info ->

                match Mods.to_interlude_rate_and_mods replay_info.ModsUsed with
                | None -> () // score is invalid for import in some way, skip
                | Some (rate2, mods) ->

                let combined_rate = rate2 * rate
                if MathF.Round(combined_rate, 3) <> MathF.Round(combined_rate, 2) || combined_rate > 2.0f || combined_rate < 0.5f then
                    Logging.Info(sprintf "Skipping score with rate %.3f because this isn't supported in Interlude" combined_rate)
                else

                let replay_data = decode_replay (replay_info, chart, rate)

                let score : Score = 
                    { 
                        time = DateTime.FromFileTimeUtc(replay_info.Timestamp).ToLocalTime()
                        replay = Replay.compress replay_data
                        rate = MathF.Round(combined_rate, 2)
                        selectedMods = mods
                        layout = Layout.Layout.LeftTwo
                        keycount = chart.Keys
                    }

                sync <| fun () ->
                    let data = Scores.getOrCreateData chart
                    if data.Scores.RemoveAll(fun s -> s.time.ToUniversalTime().Ticks = score.time.ToUniversalTime().Ticks) = 0 then score_count <- score_count + 1
                    data.Scores.Add(score)

        Logging.Info(sprintf "Finished importing osu! scores (%i scores from %i maps)" score_count chart_count)

    let import_osu_scores_service = 
        { new Async.Service<unit, unit>() with
            override this.Handle(()) =
                async { import_osu_scores() }
        }