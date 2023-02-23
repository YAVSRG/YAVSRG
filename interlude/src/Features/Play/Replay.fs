namespace Interlude.Features.Play

open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Prelude.Gameplay.Mods
open Prelude.Charts.Formats.Interlude
open Prelude.Scoring
open Prelude.Scoring.Metrics
open Interlude.Content
open Interlude.UI
open Interlude.Features
open Interlude.Features.Play.GameplayWidgets

[<RequireQualifiedAccess>]
type ReplayMode =
    | Auto
    | Replay of chart: ModChart * rate: float32 * ReplayData

module ReplayScreen =

    let replay_screen(mode: ReplayMode) =

        let replay_data, is_auto, rate, chart =
            match mode with
            | ReplayMode.Auto -> 
                let chart = Gameplay.Chart.withMods.Value
                StoredReplayProvider.AutoPlay (chart.Keys, chart.Notes) :> IReplayProvider,
                true,
                Gameplay.rate.Value,
                chart
            | ReplayMode.Replay (modchart, rate, data) -> 
                StoredReplayProvider(data) :> IReplayProvider,
                false,
                rate,
                modchart
        Gameplay.rate.Value <- rate

        let firstNote = offsetOf chart.Notes.First.Value
        let ruleset = Rulesets.current
        let scoring = createScoreMetric ruleset chart.Keys replay_data chart.Notes rate

        { new IPlayScreen(chart, PacemakerInfo.None, ruleset, scoring) with
            override this.AddWidgets() =
                let inline add_widget x = add_widget (this, this.Playfield, this.State) x
                
                if not is_auto then
                    add_widget AccuracyMeter
                    add_widget HitMeter
                    add_widget LifeMeter
                    add_widget JudgementCounts
                add_widget ComboMeter
                add_widget SkipButton
                add_widget ProgressMeter

            override this.Update(elapsedTime, bounds) =
                base.Update(elapsedTime, bounds)
                let now = Song.timeWithOffset()
                let chartTime = now - firstNote
            
                if not replay_data.Finished then scoring.Update chartTime
            
                if (!|"options").Tapped() then
                    QuickOptions.show(scoring, ignore)
                    
                if replay_data.Finished then Screen.back Transitions.Flags.Default
        }