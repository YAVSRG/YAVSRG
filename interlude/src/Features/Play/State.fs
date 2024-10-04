namespace Interlude.Features.Play

open System
open Prelude
open Prelude.Charts
open Prelude.Charts.Processing
open Prelude.Gameplay.Replays
open Prelude.Gameplay.Scoring
open Percyqaz.Flux.Audio
open Interlude.Content
open Interlude.Features.Gameplay
open Interlude.Features.Pacemaker

type PlayState =
    {
        Chart: Chart
        WithColors: ColoredChart
        mutable Scoring: ScoreProcessor
        ScoringChanged: Event<unit>
        CurrentChartTime: unit -> ChartTime
        Pacemaker: PacemakerState
    }
    // todo: remove references to SelectedChart.rate.Value in all HUD elements -> member this.Rate = this.Scoring.Rate
    member this.Ruleset = this.Scoring.Ruleset

    static member Dummy(info: LoadedChartInfo) =
        let replay_data: IReplayProvider = StoredReplayProvider.AutoPlay(info.WithColors.Keys, info.WithColors.Source.Notes)
        let ruleset = Rulesets.current
        let scoring = ScoreProcessor.create ruleset info.WithColors.Keys replay_data info.WithColors.Source.Notes SelectedChart.rate.Value
        let first_note = info.WithMods.FirstNote

        {
            Chart = info.Chart
            WithColors = info.WithColors
            Scoring = scoring
            ScoringChanged = Event<unit>()
            CurrentChartTime = fun () -> Song.time_with_offset () - first_note
            Pacemaker = PacemakerState.None
        }

    member this.SubscribeEvents(handler: GameplayEvent -> unit) =
        let mutable obj: IDisposable = this.Scoring.OnEvent.Subscribe handler

        this.ScoringChanged.Publish.Add(fun () ->
            obj.Dispose()
            obj <- this.Scoring.OnEvent.Subscribe handler
        )

    member this.ChangeScoring(scoring) =
        this.Scoring <- scoring
        this.ScoringChanged.Trigger()
