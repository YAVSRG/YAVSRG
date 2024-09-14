namespace Interlude.Features.Gameplay

open Percyqaz.Common
open Prelude.Charts.Processing
open Prelude.Gameplay.Mods
open Prelude.Gameplay
open Prelude.Data.Library.Caching
open Prelude.Data
open Interlude.Content
open Interlude.Options
open Interlude.Features.Online
open Interlude.Web.Shared.Requests

// todo: consider making 'Gameplay' folder into some kind of State management folder, outside of Features

module Gameplay =

    let score_info_from_gameplay
        (info: LoadedChartInfo)
        (scoring: ScoreProcessor)
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
        let mod_status = score_info.ModStatus

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

                let standardised_score =
                    if Rulesets.current_hash <> Rulesets.DEFAULT_HASH then
                        score_info.WithRuleset Rulesets.DEFAULT
                    else score_info

                KeymodeSkillBreakdown.score standardised_score.Patterns.Patterns standardised_score.Accuracy standardised_score.Rate Skillsets.keymode_skills.[standardised_score.WithMods.Keys - 3]
                |> ignore //|> printfn "%O"

                let new_bests, improvement_flags =
                    match Map.tryFind Rulesets.current_hash save_data.PersonalBests with
                    | Some existing_bests -> Bests.update score_info existing_bests
                    | None -> Bests.create score_info, ImprovementFlags.New

                if not options.OnlySaveNewRecords.Value || improvement_flags <> ImprovementFlags.None then
                    UserDatabase.save_score score_info.CachedChart.Hash (ScoreInfo.to_score score_info) Content.UserData
                    save_data.PersonalBests <- Map.add Rulesets.current_hash new_bests save_data.PersonalBests

                    if Rulesets.current_hash <> Rulesets.DEFAULT_HASH then
                        let new_standard_bests =
                            match Map.tryFind Rulesets.DEFAULT_HASH save_data.PersonalBests with
                            | Some existing_bests -> Bests.update standardised_score existing_bests |> fst
                            | None -> Bests.create standardised_score
                        save_data.PersonalBests <- Map.add Rulesets.DEFAULT_HASH new_standard_bests save_data.PersonalBests

                    UserDatabase.save_changes Content.UserData
                improvement_flags

            else
                UserDatabase.save_score score_info.CachedChart.Hash (ScoreInfo.to_score score_info) Content.UserData
                ImprovementFlags.None
        else
            ImprovementFlags.None

    let mutable watch_replay: ScoreInfo * ColoredChart -> unit = ignore
    let mutable continue_endless_mode: unit -> bool = fun () -> false
    let mutable retry: unit -> unit = ignore