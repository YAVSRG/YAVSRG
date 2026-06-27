namespace Prelude.Tests.Helpers

open Prelude.Gameplay.Scoring

type ScoringEventCollector(ruleset, keys, replay, notes, rate) as this =
    inherit ScoreProcessor(ruleset, keys, replay, notes, rate)

    let events = ResizeArray<GameplayEvent>()

    do
        this.OnEvent.Add(fun event ->
            printfn "%A" event
            events.Add event
        )

    member this.Events = events.AsReadOnly()