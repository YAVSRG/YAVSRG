namespace Prelude.Tests.Scoring // todo: rename to Rulesets

open NUnit.Framework
open Prelude
open Prelude.Gameplay
open Prelude.Gameplay.RulesetsV2
open Prelude.Gameplay.ScoringV2
open Helpers

module Events =

    let RULESET = SC.create 4

    [<Test>]
    let BasicEndToEnd () =
        let notes = 
            ChartBuilder(4)
                .Note(0.0f<ms>)
                .Note(1000.0f<ms>)
                .Build()

        let replay = 
            ReplayBuilder()
                .KeyDownFor(0.0f<ms>, 30.0f<ms>, 0)
                .KeyDownFor(500.0f<ms>, 30.0f<ms>, 0)
                .KeyDownFor(1000.0f<ms>, 30.0f<ms>, 0)
                .Build()

        let event_processing = GameplayEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity
        Assert.Pass()

    [<Test>]
    let TapNotes_LateOffsets () =
        let notes = 
            ChartBuilder(4)
                .Note(0.0f<ms>)
                .Note(1000.0f<ms>)
                .Note(2000.0f<ms>)
                .Note(3000.0f<ms>)
                .Build()

        let replay = 
            ReplayBuilder()
                .KeyDown(0.0f<ms>, 0)
                .KeyUp(250.0f<ms>, 0)
                .KeyDown(1010.0f<ms>, 0)
                .KeyUp(1250.0f<ms>, 0)
                .KeyDown(2020.0f<ms>, 0)
                .KeyUp(2250.0f<ms>, 0)
                .KeyDown(3030.0f<ms>, 0)
                .Build()

        let event_processing = GameplayEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                HIT(0.0f<ms / rate>, false)
                HIT(10.0f<ms / rate>, false)
                HIT(20.0f<ms / rate>, false)
                HIT(30.0f<ms / rate>, false)
            ],
            event_processing.Events |> Seq.map _.Action
        )
    
    [<Test>]
    let TapNotes_EarlyOffsets () =
        let notes = 
            ChartBuilder(4)
                .Note(0.0f<ms>)
                .Note(1000.0f<ms>)
                .Note(2000.0f<ms>)
                .Note(3000.0f<ms>)
                .Build()

        let replay = 
            ReplayBuilder()
                .KeyDown(0.0f<ms>, 0)
                .KeyUp(250.0f<ms>, 0)
                .KeyDown(990.0f<ms>, 0)
                .KeyUp(1250.0f<ms>, 0)
                .KeyDown(1980.0f<ms>, 0)
                .KeyUp(2250.0f<ms>, 0)
                .KeyDown(2970.0f<ms>, 0)
                .Build()

        let event_processing = GameplayEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                HIT(0.0f<ms / rate>, false)
                HIT(-10.0f<ms / rate>, false)
                HIT(-20.0f<ms / rate>, false)
                HIT(-30.0f<ms / rate>, false)
            ],
            event_processing.Events |> Seq.map _.Action
        )
    
    [<Test>]
    let GhostTap_BetweenNotes () =
        let notes = 
            ChartBuilder(4)
                .Note(0.0f<ms>)
                .Note(1000.0f<ms>)
                .Build()

        let replay = 
            ReplayBuilder()
                .KeyDownFor(0.0f<ms>, 30.0f<ms>, 0)
                .KeyDownFor(500.0f<ms>, 30.0f<ms>, 0)
                .KeyDownFor(1000.0f<ms>, 30.0f<ms>, 0)
                .Build()

        let event_processing = GameplayEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                HIT(0.0f<ms / rate>, false)
                GHOST_TAP
                HIT(0.0f<ms / rate>, false)
            ],
            event_processing.Events |> Seq.map _.Action
        )
    
    [<Test(Description = "Extra inputs before the first note should not count as ghost taps")>]
    let GhostTap_ImpossibleBeforeNotes () =
        let notes = 
            ChartBuilder(4)
                .Note(0.0f<ms>)
                .Note(1000.0f<ms>)
                .Build()

        let replay = 
            ReplayBuilder()
                .KeyDownFor(-1000.0f<ms>, 30.0f<ms>, 0)
                .KeyDownFor(0.0f<ms>, 30.0f<ms>, 0)
                .KeyDownFor(1000.0f<ms>, 30.0f<ms>, 0)
                .Build()

        let event_processing = GameplayEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.False(
            event_processing.Events 
            |> Seq.exists (fun ev -> ev.Action = GHOST_TAP)
        )

        Assert.AreEqual(
            [
                HIT(0.0f<ms / rate>, false)
                HIT(0.0f<ms / rate>, false)
            ],
            event_processing.Events |> Seq.map _.Action
        )
    
    [<Test(Description = "Extra inputs after the last note still count as ghost taps")>]
    let GhostTap_AfterNotes () =
        let notes = 
            ChartBuilder(4)
                .Note(0.0f<ms>)
                .Note(1000.0f<ms>)
                .Build()

        let replay = 
            ReplayBuilder()
                .KeyDownFor(0.0f<ms>, 30.0f<ms>, 0)
                .KeyDownFor(1000.0f<ms>, 30.0f<ms>, 0)
                .KeyDownFor(2000.0f<ms>, 30.0f<ms>, 0)
                .Build()

        let event_processing = GameplayEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                HIT(0.0f<ms / rate>, false)
                HIT(0.0f<ms / rate>, false)
                GHOST_TAP
            ],
            event_processing.Events |> Seq.map _.Action
        )