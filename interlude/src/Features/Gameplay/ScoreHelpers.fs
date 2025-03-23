namespace Interlude.Features.Gameplay

open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Prelude
open Prelude.Mods
open Prelude.Gameplay.Replays
open Prelude.Gameplay.Scoring
open Prelude.Gameplay.Rulesets
open Prelude.Calculator
open Prelude.Skins.Noteskins
open Prelude.Data.User
open Prelude.Data.User.Stats
open Prelude.Data.Library
open Interlude.Content
open Interlude.Options
open Interlude.UI
open Interlude.Features.Online
open Interlude.Web.Shared.Requests

// todo: consider making 'Gameplay' folder into some kind of State management folder, outside of Features

module Gameplay =

    let private score_saved_ev = Event<ScoreInfo>()
    let score_saved = score_saved_ev.Publish

    let private score_deleted_ev = Event<int64>()
    let score_deleted = score_deleted_ev.Publish

    let private leaderboard_rank_changed_ev = Event<ScoreInfo>()
    let leaderboard_rank_changed = leaderboard_rank_changed_ev.Publish

    let upload_score (score_info: ScoreInfo) =
        Charts.Scores.Save.post (
            ({
                ChartId = score_info.ChartMeta.Hash
                Replay = score_info.Replay |> Replay.compress_string
                Rate = score_info.Rate
                Mods = score_info.Mods
                Timestamp = score_info.TimePlayed
            }),
            (function
                | None -> Logging.Error "Error submitting score (%s on %s)" (score_info.Ruleset.FormatAccuracy score_info.Accuracy) score_info.ChartMeta.Title
                | Some None -> ()
                | Some (Some leaderboard_result) ->
                match leaderboard_result.LeaderboardPosition with
                | Some new_position ->
                    Logging.Info "New leaderboard rank! (%s on %s) is #%i"
                        (score_info.Ruleset.FormatAccuracy score_info.Accuracy) score_info.ChartMeta.Title
                        new_position
                    GameThread.defer (fun () -> leaderboard_rank_changed_ev.Trigger score_info)
                | None -> ()
            )
        )

    let score_info_from_gameplay
        (info: LoadedChartInfo)
        (scoring: ScoreProcessor)
        (replay_data: ReplayData)
        (failed: bool)
        : ScoreInfo =
        {
            ChartMeta = info.ChartMeta
            Chart = info.Chart
            WithMods = info.WithMods

            PlayedBy = ScorePlayedBy.You
            TimePlayed = Timestamp.now ()
            Rate = SelectedChart.rate.Value

            Replay = replay_data
            Scoring = scoring
            Lamp = Lamp.calculate scoring.Ruleset.Lamps scoring.JudgementCounts scoring.ComboBreaks
            Grade = Grade.calculate scoring.Ruleset.Grades scoring.Accuracy

            Rating = info.Difficulty
            Physical = Performance.calculate info.Difficulty scoring

            ImportedFromOsu = false
            IsFailed = failed
        }

    let set_score (quit_out: bool) (score_info: ScoreInfo) (save_data: ChartSaveData) : ImprovementFlags * SessionXPGain option =
        let mod_status = score_info.ModStatus

        if
            mod_status < ModStatus.Unstored
        then
            if mod_status = ModStatus.Ranked then

                let standardised_score =
                    if Rulesets.current_hash <> SC_J4_HASH then
                        score_info.WithRuleset SC_J4
                    else score_info

                if Network.status = Network.Status.LoggedIn && not standardised_score.IsFailed then
                    upload_score standardised_score

                let new_bests, improvement_flags =
                    match Map.tryFind Rulesets.current_hash save_data.PersonalBests with
                    | Some existing_bests -> Bests.update score_info existing_bests
                    | None -> Bests.create score_info, ImprovementFlags.New

                let xp_gain =
                    if quit_out then
                        Stats.quitter_penalty Content.UserData
                    else
                        Stats.handle_score standardised_score improvement_flags Content.UserData

                if (options.QuitOutBehaviour.Value = QuitOutBehaviour.SaveAndShow || not quit_out) && (not options.OnlySaveNewRecords.Value || improvement_flags <> ImprovementFlags.None) then
                    UserDatabase.save_score score_info.ChartMeta.Hash (ScoreInfo.to_score score_info) Content.UserData
                    score_saved_ev.Trigger score_info
                    save_data.PersonalBests <- Map.add Rulesets.current_hash new_bests save_data.PersonalBests

                    if Rulesets.current_hash <> SC_J4_HASH then
                        let new_standard_bests =
                            match Map.tryFind SC_J4_HASH save_data.PersonalBests with
                            | Some existing_bests -> Bests.update standardised_score existing_bests |> fst
                            | None -> Bests.create standardised_score
                        save_data.PersonalBests <- Map.add SC_J4_HASH new_standard_bests save_data.PersonalBests

                    UserDatabase.save_changes Content.UserData
                improvement_flags, Some xp_gain

            elif (options.QuitOutBehaviour.Value = QuitOutBehaviour.SaveAndShow || not quit_out) && not options.OnlySaveNewRecords.Value then
                UserDatabase.save_score score_info.ChartMeta.Hash (ScoreInfo.to_score score_info) Content.UserData
                score_saved_ev.Trigger score_info
                ImprovementFlags.None, None

            else
                ImprovementFlags.None, None
        else
            ImprovementFlags.None, None

    let delete_score (score_info: ScoreInfo) =
        let score_name =
            sprintf "%s | %s" score_info.Scoring.FormattedAccuracy (score_info.Ruleset.LampName score_info.Lamp)

        if UserDatabase.delete_score score_info.ChartMeta.Hash score_info.TimePlayed Content.UserData then
            score_deleted_ev.Trigger score_info.TimePlayed
            Notifications.action_feedback (Icons.TRASH, [ score_name ] %> "notification.deleted", "")
        else
            Logging.Debug("Couldn't find score matching timestamp to delete")

    let mutable watch_replay: ScoreInfo * ColoredChart -> unit = ignore
    let mutable continue_endless_mode: unit -> bool = fun () -> false
    let mutable retry: unit -> unit = ignore