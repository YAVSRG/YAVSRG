namespace Interlude.Features.Play

open System
open Prelude.Charts
open Prelude.Skins.Noteskins
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

    member this.SubscribeEvents(handler: GameplayEvent -> unit) =
        let mutable obj: IDisposable = this.Scoring.OnEvent.Subscribe handler

        this.ScoringChanged.Publish.Add(fun () ->
            obj.Dispose()
            obj <- this.Scoring.OnEvent.Subscribe handler
        )

    member this.ChangeScoring(scoring: ScoreProcessor) =
        this.Scoring <- scoring
        this.ScoringChanged.Trigger()

    static member Dummy(info: LoadedChartInfo) : PlayState * (unit -> unit) =
        let replay_data = Replay.perfect_replay info.WithColors.Keys info.WithColors.Source.Notes
        let ruleset = Rulesets.current
        let scoring = ScoreProcessor.create ruleset info.WithColors.Keys (StoredReplayProvider replay_data) info.WithColors.Source.Notes SelectedChart.rate.Value
        let first_note = info.WithMods.FirstNote

        let state =
            {
                Chart = info.Chart
                WithColors = info.WithColors
                Scoring = scoring
                ScoringChanged = Event<unit>()
                CurrentChartTime = fun () -> Song.time_with_offset () - first_note
                Pacemaker = PacemakerState.None
            }

        scoring.Update (state.CurrentChartTime())

        let reset () =
            let recreated = ScoreProcessor.create ruleset info.WithColors.Keys (StoredReplayProvider replay_data) info.WithColors.Source.Notes SelectedChart.rate.Value
            recreated.Update (state.CurrentChartTime())
            state.ChangeScoring recreated

        state, reset