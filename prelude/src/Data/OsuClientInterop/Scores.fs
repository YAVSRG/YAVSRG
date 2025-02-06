namespace Prelude.Data.OsuClientInterop

open System
open System.IO
open System.Text
open Percyqaz.Common
open Prelude
open Prelude.Charts
open Prelude.Charts.Conversions
open Prelude.Charts.Formats.osu
open Prelude.Gameplay.Replays
open Prelude.Data.User
open Prelude.Data.Library
open Prelude.Data.Library.Imports

module Scores =

    type ImportRequest =
        {
            UserDatabase: UserDatabase
            ChartDatabase: ChartDatabase
            OsuRootPath: string
        }

    let private find_matching_chart (chart_db: ChartDatabase) (beatmap_data: OsuDatabase_Beatmap) (converted_osu_chart: Chart) =
        let chart_hash = Chart.hash converted_osu_chart

        match ChartDatabase.get_meta chart_hash chart_db with
        | None ->
            match detect_rate_mod beatmap_data.Difficulty with
            | Some rate ->
                let chart = Chart.scale rate converted_osu_chart
                let chart_hash = Chart.hash chart

                match ChartDatabase.get_meta chart_hash chart_db with
                | None ->
                    Logging.Warn
                        "Skipping %.2fx of %s [%s], can't find a matching imported 1.00x chart"
                        rate
                        beatmap_data.TitleUnicode
                        beatmap_data.Difficulty
                    None
                | Some _ -> Some(chart, chart_hash, rate)
            | None ->
                Logging.Warn "%s [%s] skipped, can't find a matching imported chart" beatmap_data.TitleUnicode beatmap_data.Difficulty
                None
        | Some _ -> Some(converted_osu_chart, chart_hash, 1.0f<rate>)

    let private import_osu_score (osu_root_folder: string) (user_db: UserDatabase) (score: OsuReplay) (chart: Chart, chart_hash: string, chart_rate: Rate) : bool =
        let replay_file =
            Path.Combine(
                osu_root_folder,
                "Data",
                "r",
                sprintf "%s-%i.osr" score.BeatmapHash score.Timestamp
            )

        match OsuReplay.TryReadFile replay_file with
        | None -> false
        | Some replay_info ->

        match Mods.to_interlude_rate_and_mods replay_info.ModsUsed with
        | None -> false // score is invalid for import in some way, skip
        | Some(rate_via_mods, mods) ->

        let combined_rate = float32 (rate_via_mods * chart_rate)

        if
            MathF.Round(combined_rate, 3) <> MathF.Round(combined_rate, 2)
            || combined_rate > 3.0f
            || combined_rate < 0.5f
        then
            Logging.Info "Skipping score with rate %.3f because this isn't supported in Interlude" combined_rate
            false
        else

        let replay_data = OsuReplay.decode (replay_info, chart.FirstNote, chart_rate)

        let score: Score =
            {
                Timestamp =
                    DateTime.FromFileTimeUtc(replay_info.Timestamp).ToLocalTime()
                    |> Timestamp.from_datetime
                Replay = Replay.compress_bytes replay_data
                Rate = MathF.Round(combined_rate, 2) * 1.0f<rate>
                Mods = mods
                IsImported = true
                IsFailed = false
                Keys = chart.Keys
            }

        let existing_score_replaced = UserDatabase.delete_score chart_hash score.Timestamp user_db
        UserDatabase.save_score chart_hash score user_db

        not existing_score_replaced

    let private import_osu_scores (osu_root_folder: string) (user_db: UserDatabase) (chart_db: ChartDatabase) : int * int =

        let scores =
            use file = Path.Combine(osu_root_folder, "scores.db") |> File.OpenRead
            Logging.Info "Reading scores database .."
            use reader = new BinaryReader(file, Encoding.UTF8)
            OsuScoreDatabase.Read(reader)

        Logging.Info "Read score data, containing info about %i maps" scores.Beatmaps.Length

        let main_db =
            use file = Path.Combine(osu_root_folder, "osu!.db") |> File.OpenRead
            Logging.Info "Reading osu! database .."
            use reader = new BinaryReader(file, Encoding.UTF8)
            OsuDatabase.Read(reader)

        Logging.Info
            "Read %s's osu! database containing %i maps, starting import .."
            main_db.PlayerName
            main_db.Beatmaps.Length

        let chart_map =
            main_db.Beatmaps
            |> Seq.filter (fun b -> b.Mode = 3uy)
            |> Seq.map (fun b -> b.Hash, b)
            |> Map.ofSeq

        let mutable chart_count = 0
        let mutable score_count = 0

        for beatmap_score_data in
            scores.Beatmaps
            |> Seq.where (fun b -> b.Scores.Length > 0 && b.Scores.[0].Mode = 3uy) do
            match Map.tryFind beatmap_score_data.Hash chart_map with
            | None -> ()
            | Some beatmap_data ->

            let osu_file =
                Path.Combine(osu_root_folder, "Songs", beatmap_data.FolderName, beatmap_data.Filename)

            match
                match Beatmap.FromFile osu_file with
                | Ok beatmap ->
                    Osu_To_Interlude.convert
                        beatmap
                        {
                            Config = ConversionOptions.Default
                            Source = osu_file
                        }
                    |> function Ok i -> Ok i | Error s -> Error (snd s)
                | Error reason -> Error reason
            with
            | Error reason -> Logging.Warn "%s [%s] skipped, conversion failed: %s" beatmap_data.TitleUnicode beatmap_data.Difficulty reason
            | Ok chart ->

            match find_matching_chart chart_db beatmap_data chart.Chart with
            | None -> ()
            | Some(chart, chart_hash, rate) ->

            chart_count <- chart_count + 1

            for score in beatmap_score_data.Scores do
                if import_osu_score osu_root_folder user_db score (chart, chart_hash, rate) then
                    score_count <- score_count + 1

        Logging.Info "Finished importing osu! scores (%i scores from %i maps)" score_count chart_count
        score_count, chart_count

    let import_osu_scores_service =
        { new Async.Service<ImportRequest, int * int>() with
            override this.Handle(request) =
                async {
                    return import_osu_scores request.OsuRootPath request.UserDatabase request.ChartDatabase
                }
        }