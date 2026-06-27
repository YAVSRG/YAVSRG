namespace Prelude.Tests.Helpers

open Prelude.Gameplay.Scoring

type GameplayEventCollector(ruleset, keys, replay, notes, rate) =
    inherit GameplayEventProcessor(ruleset, keys, replay, notes, rate)

    let events = ResizeArray<GameplayEventInternal>()

    override this.HandleEvent(event) =
        printfn "%A" event
        events.Add event

    member this.Events = events.AsReadOnly()