namespace Prelude.Tests.Helpers

open Prelude
open Prelude.Charts
open Prelude.Gameplay.Replays
open Prelude.Gameplay.Rulesets
open Prelude.Gameplay.Scoring

type GameplayEventCollector(ruleset: Ruleset, replay: ReplaySource, note_data: NoteData, rate: Rate) =
    inherit GameplayEventProcessor(ruleset, replay, note_data, rate)

    let events = ResizeArray<GameplayEvent>()

    override this.HandleEvent(event: GameplayEvent) : unit =
        printfn "%A" event
        events.Add event

    member this.Events = events.AsReadOnly()