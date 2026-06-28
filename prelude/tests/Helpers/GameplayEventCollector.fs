namespace Prelude.Tests.Helpers

open Prelude
open Prelude.Charts
open Prelude.Gameplay.Replays
open Prelude.Gameplay.Rulesets
open Prelude.Gameplay.Scoring

type GameplayEventCollector(ruleset: Ruleset, replay: ReplaySource, note_data: NoteData, rate: Rate) =
    inherit GameplayEventProcessor(ruleset, note_data.Keys, replay, note_data.Notes, rate)

    let events = ResizeArray<GameplayEventInternal>()

    override this.HandleEvent(event) =
        printfn "%A" event
        events.Add event

    member this.Events = events.AsReadOnly()