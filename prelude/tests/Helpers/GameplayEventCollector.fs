namespace Prelude.Tests.Helpers

open Prelude
open Prelude.Charts
open Prelude.Gameplay.Replays
open Prelude.Gameplay.Rulesets
open Prelude.Gameplay.Scoring

type GameplayEventCollector(cached_ruleset: CachedRuleset, replay: ReplaySource, note_data: NoteData, rate: Rate) =
    inherit GameplayEventProcessor(cached_ruleset, replay, note_data, rate)

    let events = ResizeArray<GameplayEvent>()

    new(ruleset, replay, note_data, rate) =
        GameplayEventCollector(CachedRuleset.Create(ruleset), replay, note_data, rate)

    override this.HandleEvent(event: GameplayEvent) : unit =
        printfn "%A" event
        events.Add(event)

    member this.Events = events.AsReadOnly()
