namespace Prelude.Tests.Helpers

open Prelude
open Prelude.Charts
open Prelude.Gameplay.Replays
open Prelude.Gameplay.Rulesets
open Prelude.Gameplay.Scoring

type ScoringEventCollector(cached_ruleset: CachedRuleset, replay: ReplaySource, note_data: NoteData, rate: Rate) as this
    =
    inherit ScoreProcessor(cached_ruleset, replay, note_data, rate)

    let events = ResizeArray<ScoringEvent>()

    do
        this.OnEvent.Add(fun event ->
            printfn "%A" event
            events.Add(event)
        )

    new(ruleset, replay, note_data, rate)= ScoringEventCollector(CachedRuleset.Create(ruleset), replay, note_data, rate)

    member this.Events = events.AsReadOnly()
