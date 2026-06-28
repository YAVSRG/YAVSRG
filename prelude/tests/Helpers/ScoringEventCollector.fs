namespace Prelude.Tests.Helpers

open Prelude
open Prelude.Charts
open Prelude.Gameplay.Replays
open Prelude.Gameplay.Rulesets
open Prelude.Gameplay.Scoring

type ScoringEventCollector(ruleset: Ruleset, replay: ReplaySource, note_data: NoteData, rate: Rate) as this =
    inherit ScoreProcessor(ruleset, replay, note_data, rate)

    let events = ResizeArray<GameplayEvent>()

    do
        this.OnEvent.Add(fun event ->
            printfn "%A" event
            events.Add event
        )

    member this.Events = events.AsReadOnly()