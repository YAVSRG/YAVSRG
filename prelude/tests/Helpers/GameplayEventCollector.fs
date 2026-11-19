namespace Prelude.Tests.Helpers

open Prelude
open Prelude.Charts
open Prelude.Gameplay.Replays
open Prelude.Gameplay.Rulesets
open Prelude.Gameplay.Scoring

type GameplayEventCollector(ruleset_info: RulesetInfo, replay: ReplaySource, note_data: NoteData, rate: Rate) =
    inherit GameplayEventProcessor(ruleset_info, replay, note_data, rate)

    let events = ResizeArray<GameplayEvent>()

    new(ruleset: Ruleset, replay: ReplaySource, note_data: NoteData, rate: Rate) =
        GameplayEventCollector(ruleset.ToInfo(), replay, note_data, rate)

    override this.HandleEvent(event: GameplayEvent) : unit =
        printfn "%A" event
        events.Add(event)

    member this.Events = events.AsReadOnly()
