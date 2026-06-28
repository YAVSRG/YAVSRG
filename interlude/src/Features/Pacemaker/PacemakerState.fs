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
type PacemakerCreationContext =
    | None
    | FromScore of ScoreInfo
    | FromUserSetting

[<RequireQualifiedAccess>]
type PacemakerState =
    | None
    | Accuracy of float
    | Replay of float * ScoreProcessor
    | Judgement of judgement: int * target_max_count: int
    | ComboBreaks of target_max_count: int
    
    member this.Description(ruleset: Ruleset) : string =
        match this with
        | PacemakerState.None -> ""
        
        | PacemakerState.Accuracy accuracy ->
            sprintf "%s %s: %s" Icons.FLAG (%"pacemaker.accuracy") (ruleset.FormatAccuracy accuracy)
            
        | PacemakerState.Replay (accuracy, _) ->
            sprintf "%s %s: %s" Icons.FLAG (%"pacemaker.vs_score") (ruleset.FormatAccuracy accuracy)
            
        | PacemakerState.Judgement (judgement, target_max_count) ->
            let judgement_name = ruleset.JudgementName judgement
            if target_max_count = 0 then
                sprintf "%s %s" Icons.FLAG ([judgement_name] %> "pacemaker.zero_judgements")
            else
                sprintf "%s %s" Icons.FLAG ([target_max_count.ToString(); judgement_name] %> "pacemaker.n_judgements")
                
        | PacemakerState.ComboBreaks target_max_count ->
            if target_max_count = 0 then
                sprintf "%s %s" Icons.FLAG (%"pacemaker.full_combo")
            else
                sprintf "%s %s" Icons.FLAG ([target_max_count.ToString()] %> "pacemaker.n_combo_breaks")
        
    member this.IsFailedMidway(scoring: ScoreProcessor) : bool =
        match this with
        | PacemakerState.None -> false
        | PacemakerState.Accuracy _ -> false
        | PacemakerState.Replay _ -> false
        
        | PacemakerState.Judgement(judgement, target_max_count) ->
            let inline has_worse_judgement() : bool =
                let rec loop (index: int) =
                    if index >= scoring.JudgementCounts.Length then false
                    elif scoring.JudgementCounts.[index] > 0 then true
                    else loop(index + 1)
                loop(judgement + 1)
            
            let actual_count = scoring.JudgementCounts.[judgement]
            (actual_count > target_max_count) || has_worse_judgement()
            
        | PacemakerState.ComboBreaks target_max_count ->
            scoring.ComboBreaks > target_max_count
            
    member this.IsFailedAtEnd(scoring: ScoreProcessor) : bool =
        match this with
        | PacemakerState.None -> false
        | PacemakerState.Accuracy x -> scoring.Accuracy < x
        
        | PacemakerState.Replay (_, vs_score) ->
            vs_score.Update Time.infinity
            scoring.Accuracy < vs_score.Accuracy
            
        | PacemakerState.Judgement(judgement, target_max_count) ->
            let inline has_worse_judgement() : bool =
                let rec loop (index: int) =
                    if index >= scoring.JudgementCounts.Length then false
                    elif scoring.JudgementCounts.[index] > 0 then true
                    else loop(index + 1)
                loop(judgement + 1)
            
            let actual_count = scoring.JudgementCounts.[judgement]
            (actual_count > target_max_count) || has_worse_judgement()
            
        | PacemakerState.ComboBreaks target_max_count ->
            scoring.ComboBreaks > target_max_count
            
    static member Create(info: LoadedChartInfo, ctx: PacemakerCreationContext) : PacemakerState =
        let inline create_from_score (score_info: ScoreInfo) =
            let replay_scoring =
                ScoreProcessor.create
                    Rulesets.current
                    score_info.WithMods.Keys
                    (StoredReplaySource(score_info.Replay))
                    score_info.WithMods.Notes
                    score_info.Rate
            PacemakerState.Replay (score_info.Accuracy, replay_scoring)
            
        let inline create_from_accuracy(pacemaker_settings: PacemakerSettings) : PacemakerState =
            let personal_best_accuracy =
                Bests.ruleset_best_above
                    Rulesets.current_hash
                    _.Accuracy
                    SelectedChart.rate.Value
                    info.SaveData.PersonalBests
                    
            let inline should_use_personal_best(best_accuracy: float) : bool =
                pacemaker_settings.PersonalBest = PacemakerPersonalBestMode.Always
                || (pacemaker_settings.PersonalBest = PacemakerPersonalBestMode.IfBetter && best_accuracy > pacemaker_settings.Accuracy)
                
            let inline use_score(final_accuracy: float, score: Score) =
                let with_mods = ModState.apply score.Mods info.Chart
                let replay_data = Replay.FromByteArray(score.Replay)
                let scoring = ScoreProcessor.create Rulesets.current with_mods.Keys (StoredReplaySource replay_data) with_mods.Notes score.Rate
                PacemakerState.Replay (final_accuracy, scoring)
                
            let inline use_accuracy(accuracy: float) =
                PacemakerState.Accuracy accuracy
            
            match personal_best_accuracy with
            | Some (best_accuracy, _, timestamp) when should_use_personal_best(best_accuracy) ->
                match info.SaveData.ScoreByTimestamp(timestamp) with
                | Some score -> use_score(best_accuracy, score)
                | _ -> use_accuracy(best_accuracy)
                
            | _ -> use_accuracy(pacemaker_settings.Accuracy)
            
        let inline create_from_lamp(pacemaker_settings: PacemakerSettings) : PacemakerState =
            let personal_best_lamp =
                Bests.ruleset_best_above
                    Rulesets.current_hash
                    _.Lamp
                    SelectedChart.rate.Value
                    info.SaveData.PersonalBests
                    
            let inline should_use_personal_best(best_lamp: int) : bool =
                pacemaker_settings.PersonalBest = PacemakerPersonalBestMode.Always
                || (pacemaker_settings.PersonalBest = PacemakerPersonalBestMode.IfBetter && best_lamp > pacemaker_settings.Lamp)
                    
            let target_lamp =
                match personal_best_lamp with
                | Some (best_lamp, _, _) when should_use_personal_best(best_lamp) -> best_lamp
                | _ -> pacemaker_settings.Lamp
                |> max 0
                |> min (Rulesets.current.Lamps.Length - 1)

            match Rulesets.current.Lamps.[target_lamp].Requirement with
            | LampRequirement.ComboBreaksAtMost n -> PacemakerState.ComboBreaks n
            | LampRequirement.JudgementAtMost (j, n) -> PacemakerState.Judgement (j, n)
        
        match ctx with
        | PacemakerCreationContext.None -> PacemakerState.None
        | PacemakerCreationContext.FromScore score_info -> create_from_score(score_info)

        | PacemakerCreationContext.FromUserSetting ->
            let pacemaker_settings =
                match options.Pacemaker.TryGetValue(Rulesets.current_hash) with
                | true, settings -> settings
                | false, _ -> PacemakerSettings.Default

            match pacemaker_settings.Mode with
            | PacemakerMode.Accuracy -> create_from_accuracy(pacemaker_settings)
            | PacemakerMode.Lamp -> create_from_lamp(pacemaker_settings)