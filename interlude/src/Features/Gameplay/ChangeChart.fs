namespace Interlude.Features.Gameplay

open Percyqaz.Common
open Percyqaz.Flux.Input
open Prelude.Charts.Processing
open Prelude.Gameplay.Mods
open Prelude.Gameplay
open Prelude.Data.Library.Caching
open Prelude.Data
open Interlude.Content
open Interlude.Options
open Interlude.Features.Online
open Interlude.Web.Shared.Requests

module Stuff =

    let score_info_from_gameplay
        (info: LoadedChartInfo)
        (scoring: ScoreMetric)
        (replay_data: ReplayData)
        : ScoreInfo =
        {
            CachedChart = info.CacheInfo
            Chart = info.Chart
            WithMods = info.WithMods

            PlayedBy = ScorePlayedBy.You
            TimePlayed = Timestamp.now ()
            Rate = SelectedChart.rate.Value

            Replay = replay_data
            Scoring = scoring
            Lamp = Lamp.calculate scoring.Ruleset.Grading.Lamps scoring.State
            Grade = Grade.calculate scoring.Ruleset.Grading.Grades scoring.State

            Rating = info.Rating
            Patterns = info.Patterns
            Physical = Performance.calculate info.Rating info.WithMods.Keys scoring |> fst

            ImportedFromOsu = false
        }

    let set_score (met_pacemaker: bool) (score_info: ScoreInfo) (save_data: ChartSaveData) : ImprovementFlags =
        let mod_status = score_info.ModStatus()

        if
            mod_status < ModStatus.Unstored
            && (options.SaveScoreIfUnderPace.Value || met_pacemaker)
        then
            if mod_status = ModStatus.Ranked then
                if Network.status = Network.Status.LoggedIn then
                    Charts.Scores.Save.post (
                        ({
                            ChartId = score_info.CachedChart.Hash
                            Replay = score_info.Replay |> Replay.compress_string
                            Rate = score_info.Rate
                            Mods = score_info.Mods
                            Timestamp = score_info.TimePlayed |> Timestamp.to_datetime
                        }),
                        ignore
                    )

                let new_bests, improvement_flags =
                    match Map.tryFind Rulesets.current_hash save_data.PersonalBests with
                    | Some existing_bests -> Bests.update score_info existing_bests
                    | None -> Bests.create score_info, ImprovementFlags.New

                if not options.OnlySaveNewRecords.Value || improvement_flags <> ImprovementFlags.None then
                    ScoreDatabase.save_score score_info.CachedChart.Hash (ScoreInfo.to_score score_info) Content.Scores
                    save_data.PersonalBests <- Map.add Rulesets.current_hash new_bests save_data.PersonalBests
                    ScoreDatabase.save_changes Content.Scores
                improvement_flags
            else
                ScoreDatabase.save_score score_info.CachedChart.Hash (ScoreInfo.to_score score_info) Content.Scores
                ImprovementFlags.None
        else
            ImprovementFlags.None

    let change_rate_hotkeys(change_rate_by: float32 -> unit) =
        if (%%"uprate_small").Tapped() then
            change_rate_by (0.01f)
        elif (%%"uprate_half").Tapped() then
            change_rate_by (0.05f)
        elif (%%"uprate").Tapped() then
            change_rate_by (0.1f)
        elif (%%"downrate_small").Tapped() then
            change_rate_by (-0.01f)
        elif (%%"downrate_half").Tapped() then
            change_rate_by (-0.05f)
        elif (%%"downrate").Tapped() then
            change_rate_by (-0.1f)

module Things =

    let mutable private suggestion_history: CachedChart list = []

    let add_current_chart_to_history () = 
        match SelectedChart.CACHE_DATA with
        | Some cc -> suggestion_history <- cc :: suggestion_history
        | None -> ()

    let previous () : CachedChart option =
        match suggestion_history with
        | x :: xs -> suggestion_history <- xs; Some x
        | _ -> None

    let has_previous() = (List.isEmpty >> not) suggestion_history