namespace Interlude.Features.Play

open System
open Percyqaz.Flux.Audio
open Prelude
open Prelude.Skins.Noteskins
open Prelude.Gameplay.Replays
open Prelude.Gameplay.Scoring
open Interlude.Content
open Interlude.Features.Gameplay
open Interlude.Features.Pacemaker

type PlayState(info: LoadedChartInfo, pacemaker: PacemakerState, scoring: ScoreProcessor, current_time: unit -> Time) =

    let mutable scoring = scoring
    let scoring_changed_ev = Event<unit>()
    let scoring_changed = scoring_changed_ev.Publish
    let first_note = info.WithMods.FirstNote

    member this.Chart = info.Chart
    member this.WithColors = info.WithColors
    member this.Scoring = scoring
    /// 0.0f<ms> = You are at the start of the audio file
    /// Negative numbers are possible, if needed to give enough time before the first note
    member this.CurrentTime() = current_time()
    /// 0.0f<ms> = You are at the very first note of the chart
    member this.CurrentChartTime() = current_time() - first_note
    member this.Pacemaker = pacemaker

    member this.Rate = this.Scoring.Rate
    member this.Ruleset = this.Scoring.Ruleset

    member this.ChangeScoring(new_scoring: ScoreProcessor) =
        scoring <- new_scoring
        scoring_changed_ev.Trigger()

    member this.OnScoringChanged(action: unit -> unit) : IDisposable =
        scoring_changed.Subscribe action

    member this.Subscribe(handler: GameplayEvent -> unit) : IDisposable =
        let mutable obj: IDisposable = scoring.OnEvent.Subscribe handler

        let resubscribe_on_change = scoring_changed.Subscribe(fun () ->
            obj.Dispose()
            obj <- scoring.OnEvent.Subscribe handler
        )

        { new IDisposable with override this.Dispose() = resubscribe_on_change.Dispose(); obj.Dispose() }

    static member Dummy(info: LoadedChartInfo) : PlayState * (unit -> unit) =
        let replay_data = Replay.perfect_replay info.WithColors.Keys info.WithColors.Source.Notes
        let ruleset = Rulesets.current
        let scoring = ScoreProcessor.create ruleset info.WithColors.Keys (StoredReplay replay_data) info.WithColors.Source.Notes SelectedChart.rate.Value

        let state = PlayState(info, PacemakerState.None, scoring, Song.time_with_offset)

        scoring.Update (state.CurrentChartTime())

        let reset () =
            let recreated = ScoreProcessor.create ruleset info.WithColors.Keys (StoredReplay replay_data) info.WithColors.Source.Notes SelectedChart.rate.Value
            recreated.Update (state.CurrentChartTime())
            state.ChangeScoring recreated

        state, reset