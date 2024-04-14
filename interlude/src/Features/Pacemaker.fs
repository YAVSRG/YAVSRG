namespace Interlude.Features.Pacemaker

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay
open Prelude.Data
open Interlude.Options
open Interlude.UI.Menu
open Interlude.Content
open Interlude.Features.Gameplay

type PacemakerOptionsPage() as this =
    inherit Page()

    let ruleset_id = Rulesets.current_hash

    let existing =
        if options.Pacemaker.ContainsKey ruleset_id then
            options.Pacemaker.[ruleset_id]
        else
            PacemakerSettings.Default

    let mode = Setting.simple existing.Mode

    let accuracy = 
        existing.Accuracy
        |> Setting.simple
        |> Setting.bound 0.0 1.0
        |> Setting.round 3

    let lamp = Setting.simple existing.Lamp

    let use_personal_best = Setting.simple existing.UsePersonalBest

    do
        let lamps =
            seq {
                let lamp_count = Rulesets.current.Grading.Lamps.Length
                for i in 0 .. lamp_count - 1 do
                    yield i, Rulesets.current.LampName i
                if lamp_count = 0 then
                    yield 0, Rulesets.current.LampName 0
            }
            |> Array.ofSeq

        this.Content(
            page_container()
            |+ PageSetting("gameplay.pacemaker.saveunderpace", Selector<_>.FromBool options.SaveScoreIfUnderPace)
                .Pos(0)
                .Tooltip(Tooltip.Info("gameplay.pacemaker.saveunderpace"))
            |+ PageSetting("gameplay.pacemaker.onlysavenewrecords", Selector<_>.FromBool options.OnlySaveNewRecords)
                .Pos(2)
                .Tooltip(Tooltip.Info("gameplay.pacemaker.onlysavenewrecords"))
            |+ PageSetting("gameplay.pacemaker.type",
                Selector([| PacemakerMode.Accuracy, %"gameplay.pacemaker.accuracy.name"; PacemakerMode.Lamp, %"gameplay.pacemaker.lamp.name" |], mode)
            )
                .Pos(5)
            |+ PageSetting("gameplay.pacemaker.use_personal_best", Selector<_>.FromBool(use_personal_best))
                .Pos(7)
                .Tooltip(Tooltip.Info("gameplay.pacemaker.use_personal_best"))
            |+ Conditional((fun () -> mode.Value = PacemakerMode.Accuracy),
                PageSetting("gameplay.pacemaker.accuracy", Slider.Percent(accuracy |> Setting.f32)) 
                    .Pos(9)
            )
            |+ Conditional((fun () -> mode.Value = PacemakerMode.Lamp),
                PageSetting("gameplay.pacemaker.lamp", Selector(lamps, lamp))
                    .Pos(9)
            )
            |>> Container
            |+ Text(
                %"gameplay.pacemaker.hint",
                Align = Alignment.CENTER,
                Position = Position.SliceBottom(100.0f).TrimBottom(40.0f)
            )
        )

    override this.Title = %"gameplay.pacemaker.name"

    override this.OnClose() =
        options.Pacemaker.[ruleset_id] <-
            {
                Accuracy = accuracy.Value
                Lamp = lamp.Value
                Mode = mode.Value
                UsePersonalBest = use_personal_best.Value
            }


[<RequireQualifiedAccess>]
type PacemakerState =
    | None
    | Accuracy of float
    | Replay of IScoreMetric
    | Judgement of target: JudgementId * max_count: int

[<RequireQualifiedAccess>]
type PacemakerCreationContext =
    | None
    | FromScore of ScoreInfo
    | FromUserSetting

module PacemakerState =

    let pacemaker_met (scoring: IScoreMetric) (state: PacemakerState) =
        match state with
        | PacemakerState.None -> true
        | PacemakerState.Accuracy x -> scoring.Value >= x
        | PacemakerState.Replay r ->
            r.Update Time.infinity
            scoring.Value >= r.Value
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

    let create (info: Chart.LoadedChartInfo) (ctx: PacemakerCreationContext) =
        match ctx with
        | PacemakerCreationContext.None -> PacemakerState.None
        | PacemakerCreationContext.FromScore score_info ->
            let replay_data = StoredReplayProvider(score_info.Replay) :> IReplayProvider

            let replay_scoring =
                Metrics.create Rulesets.current score_info.WithMods.Keys replay_data score_info.WithMods.Notes score_info.Rate

            PacemakerState.Replay replay_scoring

        | PacemakerCreationContext.FromUserSetting ->
            let setting =
                if options.Pacemaker.ContainsKey Rulesets.current_hash then
                    options.Pacemaker.[Rulesets.current_hash]
                else
                    PacemakerSettings.Default

            match setting.Mode with
            | PacemakerMode.Accuracy -> 

                if setting.UsePersonalBest then
                    match info.SaveData.PersonalBests |> Bests.ruleset_best_below Rulesets.current_hash (_.Accuracy) rate.Value with
                    | Some best_accuracy ->
                        PacemakerState.Accuracy best_accuracy
                    | None -> PacemakerState.Accuracy setting.Accuracy
                else
                    PacemakerState.Accuracy setting.Accuracy

            | PacemakerMode.Lamp ->
                
                let lamp =
                    if setting.UsePersonalBest then
                        match info.SaveData.PersonalBests |> Bests.ruleset_best_below Rulesets.current_hash (_.Lamp) rate.Value with
                        | Some best_lamp -> best_lamp
                        | None -> setting.Lamp
                    else
                        setting.Lamp

                if lamp >= Rulesets.current.Grading.Lamps.Length || lamp < 0 then
                    PacemakerState.None
                else
                    let rs_lamp = Rulesets.current.Grading.Lamps.[lamp]
                    PacemakerState.Judgement(rs_lamp.Judgement, rs_lamp.JudgementThreshold)