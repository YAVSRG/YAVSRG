namespace Interlude.Features.Pacemaker

open Prelude
open Prelude.Gameplay
open Prelude.Gameplay.Mods
open Prelude.Data
open Prelude.Data.User
open Interlude.Options
open Interlude.Content
open Interlude.Features.Gameplay

[<RequireQualifiedAccess>]
type PacemakerState =
    | None
    | Accuracy of float
    | Replay of float * ScoreProcessorBase
    | Judgement of target: JudgementId * max_count: int

[<RequireQualifiedAccess>]
type PacemakerCreationContext =
    | None
    | FromScore of ScoreInfo
    | FromUserSetting

module PacemakerState =

    let pacemaker_met (scoring: ScoreProcessorBase) (state: PacemakerState) =
        match state with
        | PacemakerState.None -> true
        | PacemakerState.Accuracy x -> scoring.Accuracy >= x
        | PacemakerState.Replay (_, r) ->
            r.Update Time.infinity
            scoring.Accuracy >= r.Accuracy
        | PacemakerState.Judgement(judgement, count) ->
            let actual =
                if judgement = -1 then
                    scoring.State.ComboBreaks
                else
                    let mutable c = scoring.State.Judgements.[judgement]

                    for j = judgement + 1 to scoring.State.Judgements.Length - 1 do
                        if scoring.State.Judgements.[j] > 0 then
                            c <- 1000000

                    c
            actual <= count

    let create (info: LoadedChartInfo) (ctx: PacemakerCreationContext) =
        match ctx with
        | PacemakerCreationContext.None -> PacemakerState.None
        | PacemakerCreationContext.FromScore score_info ->
            let replay_data = StoredReplayProvider(score_info.Replay) :> IReplayProvider

            let replay_scoring =
                ScoreProcessor.create Rulesets.current score_info.WithMods.Keys replay_data score_info.WithMods.Notes score_info.Rate

            PacemakerState.Replay (score_info.Accuracy, replay_scoring)

        | PacemakerCreationContext.FromUserSetting ->
            let setting =
                if options.Pacemaker.ContainsKey Rulesets.current_hash then
                    options.Pacemaker.[Rulesets.current_hash]
                else
                    PacemakerSettings.Default

            match setting.Mode with
            | PacemakerMode.Accuracy -> 

                if setting.UsePersonalBest then
                    match info.SaveData.PersonalBests |> Bests.ruleset_best_above Rulesets.current_hash (_.Accuracy) SelectedChart.rate.Value with
                    | Some (best_accuracy, _, timestamp) ->

                        match info.SaveData.ScoreByTimestamp timestamp with
                        | Some score ->
                            let with_mods = Mods.apply score.Mods info.Chart
                            let replay_data = score.Replay |> Replay.decompress_bytes
                            let scoring = ScoreProcessor.create Rulesets.current with_mods.Keys (StoredReplayProvider replay_data) with_mods.Notes score.Rate

                            PacemakerState.Replay (best_accuracy, scoring)

                        | None -> PacemakerState.Accuracy best_accuracy
                    | None -> PacemakerState.Accuracy setting.Accuracy
                else
                    PacemakerState.Accuracy setting.Accuracy

            | PacemakerMode.Lamp ->
                
                let lamp =
                    if setting.UsePersonalBest then
                        match info.SaveData.PersonalBests |> Bests.ruleset_best_above Rulesets.current_hash (_.Lamp) SelectedChart.rate.Value with
                        | Some (best_lamp, _, _) -> best_lamp
                        | None -> setting.Lamp
                    else
                        setting.Lamp

                if lamp >= Rulesets.current.Grading.Lamps.Length || lamp < 0 then
                    PacemakerState.None
                else
                    let rs_lamp = Rulesets.current.Grading.Lamps.[lamp]
                    PacemakerState.Judgement(rs_lamp.Judgement, rs_lamp.JudgementThreshold)