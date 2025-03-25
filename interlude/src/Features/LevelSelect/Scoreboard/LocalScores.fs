namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Prelude.Charts
open Prelude.Mods
open Prelude.Gameplay.Rulesets
open Prelude.Data.User
open Prelude.Data.Library
open Interlude.Content
open Interlude.Features.Gameplay

module LocalScores =

    type Request =
        {
            RulesetId: string
            Ruleset: Ruleset
            ChartMeta: ChartMeta
            CurrentChart: Chart
            ChartSaveData: ChartSaveData
        }
        override this.ToString() = "<local scores calculation>"

    // Can be accessed from the UI thread at any time
    // Contains scores for the current loaded chart that have been calculated for current ruleset
    let local_scores : ResizeArray<ScoreInfo> = ResizeArray()

    let private score_loaded_ev = Event<ScoreInfo>()
    let score_loaded = score_loaded_ev.Publish

    let private scores_loaded_ev = Event<unit>()
    let scores_loaded = scores_loaded_ev.Publish

    let private load_score (score_info: ScoreInfo) =
        if score_info.Ruleset <> Rulesets.current then
            score_info.Ruleset <- Rulesets.current
        local_scores.Add score_info
        score_loaded_ev.Trigger score_info

    let private finish_loading () =
        let mutable new_bests = None
        for score_info in local_scores do
            assert(score_info.Ruleset = Rulesets.current)
            if score_info.ModStatus = ModStatus.Ranked then
                new_bests <-
                    Some(
                        match new_bests with
                        | None -> Bests.create score_info
                        | Some b -> fst (Bests.update score_info b)
                    )
        match new_bests, SelectedChart.SAVE_DATA with
        | Some new_bests, Some save_data ->
            let old_bests = save_data.PersonalBests
            let new_bests = Map.add Rulesets.current_hash new_bests old_bests

            if new_bests <> old_bests then
                save_data.PersonalBests <- new_bests
                LevelSelect.refresh_details()
        | _ -> ()
        scores_loaded_ev.Trigger ()

    let score_loader =
        { new Async.CancelQueueSeq<Request, unit -> unit>() with
            member this.Process(req: Request) =
                seq {
                    for score in req.ChartSaveData.Scores do
                        let score_info = ScoreInfo.from_score req.ChartMeta req.CurrentChart req.Ruleset score
                        yield fun () -> load_score score_info
                    yield finish_loading
                }

            member this.Handle(action) = action ()
        }

    do
        SelectedChart.on_chart_change_started.Add (fun info ->
            local_scores.Clear()
            score_loader.Cancel()
        )

        SelectedChart.on_chart_change_finished.Add (fun info ->
            score_loader.Request
                {
                    RulesetId = Rulesets.current_hash
                    Ruleset = Rulesets.current
                    ChartMeta = info.ChartMeta
                    CurrentChart = info.Chart
                    ChartSaveData = info.SaveData
                }
        )

        SelectedChart.when_loaded false (fun info ->
            score_loader.Request
                {
                    RulesetId = Rulesets.current_hash
                    Ruleset = Rulesets.current
                    ChartMeta = info.ChartMeta
                    CurrentChart = info.Chart
                    ChartSaveData = info.SaveData
                }
        )

        Rulesets.on_changed.Add (fun ruleset ->
            for score_info in local_scores do
                score_info.Ruleset <- ruleset
        )

        Gameplay.score_saved.Add load_score
        Gameplay.score_deleted.Add (fun timestamp -> local_scores.RemoveAll(fun score_info -> score_info.TimePlayed = timestamp) |> ignore)