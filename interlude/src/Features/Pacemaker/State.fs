namespace Interlude.Features.Pacemaker

open Prelude
open Prelude.Mods
open Prelude.Gameplay.Replays
open Prelude.Gameplay.Rulesets
open Prelude.Gameplay.Scoring
open Prelude.Data.User
open Interlude.Options
open Interlude.Content
open Interlude.Features.Gameplay
open Interlude.UI

[<RequireQualifiedAccess>]
type PacemakerState =
    | None
    | Accuracy of float
    | Replay of float * ScoreProcessor
    | Judgement of target: int * max_count: int
    | ComboBreaks of max_count: int

[<RequireQualifiedAccess>]
type PacemakerCreationContext =
    | None
    | FromScore of ScoreInfo
    | FromUserSetting

module PacemakerState =

    let pacemaker_failed (scoring: ScoreProcessor) (state: PacemakerState) : bool =
        match state with
        | PacemakerState.None -> false
        | PacemakerState.Accuracy x -> false
        | PacemakerState.Replay (_, r) -> false
        | PacemakerState.Judgement(judgement, count) ->
            let actual =
                let mutable c = scoring.JudgementCounts.[judgement]

                for j = judgement + 1 to scoring.JudgementCounts.Length - 1 do
                    if scoring.JudgementCounts.[j] > 0 then
                        c <- System.Int32.MaxValue

                c
            actual > count
        | PacemakerState.ComboBreaks count ->
            scoring.ComboBreaks > count

    let pacemaker_met (scoring: ScoreProcessor) (state: PacemakerState) : bool =
        match state with
        | PacemakerState.None -> true
        | PacemakerState.Accuracy x -> scoring.Accuracy >= x
        | PacemakerState.Replay (_, r) ->
            r.Update Time.infinity
            scoring.Accuracy >= r.Accuracy
        | PacemakerState.Judgement(judgement, count) ->
            let actual =
                let mutable c = scoring.JudgementCounts.[judgement]

                for j = judgement + 1 to scoring.JudgementCounts.Length - 1 do
                    if scoring.JudgementCounts.[j] > 0 then
                        c <- System.Int32.MaxValue

                c
            actual <= count
        | PacemakerState.ComboBreaks count ->
            scoring.ComboBreaks <= count

    let create (info: LoadedChartInfo) (ctx: PacemakerCreationContext) : PacemakerState =
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

                match info.SaveData.PersonalBests |> Bests.ruleset_best_above Rulesets.current_hash (_.Accuracy) SelectedChart.rate.Value with
                | Some (best_accuracy, _, timestamp)
                    when setting.PersonalBest = PacemakerPersonalBestMode.Always
                    || (setting.PersonalBest = PacemakerPersonalBestMode.IfBetter && best_accuracy > setting.Accuracy) ->

                    match info.SaveData.ScoreByTimestamp timestamp with
                    | Some score ->
                        let with_mods = ModState.apply score.Mods info.Chart
                        let replay_data = score.Replay |> Replay.decompress_bytes
                        let scoring = ScoreProcessor.create Rulesets.current with_mods.Keys (StoredReplayProvider replay_data) with_mods.Notes score.Rate

                        PacemakerState.Replay (best_accuracy, scoring)

                    | None -> PacemakerState.Accuracy best_accuracy
                | _ -> PacemakerState.Accuracy setting.Accuracy

            | PacemakerMode.Lamp ->

                let lamp =
                    match info.SaveData.PersonalBests |> Bests.ruleset_best_above Rulesets.current_hash (_.Lamp) SelectedChart.rate.Value with
                    | Some (best_lamp, _, _)
                        when setting.PersonalBest = PacemakerPersonalBestMode.Always
                        || (setting.PersonalBest = PacemakerPersonalBestMode.IfBetter && best_lamp > setting.Lamp) ->
                        max 0 best_lamp
                    | _ -> setting.Lamp

                if lamp >= Rulesets.current.Lamps.Length || lamp < 0 then
                    PacemakerState.None
                else
                    match Rulesets.current.Lamps.[lamp].Requirement with
                    | LampRequirement.ComboBreaksAtMost n -> PacemakerState.ComboBreaks n
                    | LampRequirement.JudgementAtMost (j, n) -> PacemakerState.Judgement (j, n)

    let description (pacemaker: PacemakerState) : string =
        match pacemaker with
        | PacemakerState.None -> ""
        | PacemakerState.Accuracy acc ->
            sprintf "%s %s: %s" Icons.FLAG (%"pacemaker.accuracy") (Rulesets.current.FormatAccuracy acc)
        | PacemakerState.Replay (acc, _) -> sprintf "%s %s: %s" Icons.FLAG (%"pacemaker.vs_score") (Rulesets.current.FormatAccuracy acc)
        | PacemakerState.Judgement (j, count) ->
            let jname = Rulesets.current.JudgementName j
            if count = 0 then
                sprintf "%s %s" Icons.FLAG ([jname] %> "pacemaker.zero_judgements")
            else
                sprintf "%s %s" Icons.FLAG ([count.ToString(); jname] %> "pacemaker.n_judgements")
        | PacemakerState.ComboBreaks count ->
            if count = 0 then
                sprintf "%s %s" Icons.FLAG (%"pacemaker.full_combo")
            else
                sprintf "%s %s" Icons.FLAG ([count.ToString()] %> "pacemaker.n_combo_breaks")