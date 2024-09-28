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
                .KeyDownFor(0.0f<ms>, 30.0f<ms>)
                .KeyDownFor(500.0f<ms>, 30.0f<ms>)
                .KeyDownFor(1000.0f<ms>, 30.0f<ms>)
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
                .KeyDownFor(0.0f<ms>, 250.0f<ms>)
                .KeyDownFor(1010.0f<ms>, 250.0f<ms>)
                .KeyDownFor(2020.0f<ms>, 250.0f<ms>)
                .KeyDownFor(3030.0f<ms>, 250.0f<ms>)
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
                .KeyDownFor(0.0f<ms>, 250.0f<ms>)
                .KeyDownFor(990.0f<ms>, 250.0f<ms>)
                .KeyDownFor(1980.0f<ms>, 250.0f<ms>)
                .KeyDownFor(2970.0f<ms>, 250.0f<ms>)
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
    let TapNotes_SplitChords () =
        let notes = 
            ChartBuilder(4)
                .Note(0.0f<ms>, 0)
                .Note(0.0f<ms>, 1)
                .Note(0.0f<ms>, 2)
                .Note(0.0f<ms>, 3)
                .Note(1000.0f<ms>, 0)
                .Note(1000.0f<ms>, 1)
                .Note(1000.0f<ms>, 2)
                .Note(1000.0f<ms>, 3)
                .Build()

        let replay = 
            ReplayBuilder()
                .KeyDown(-20.0f<ms>, 0)
                .KeyDown(-5.0f<ms>, 1)
                .KeyDown(5.0f<ms>, 2)
                .KeyDown(20.0f<ms>, 3)
                .KeyUp(50.0f<ms>, 0)
                .KeyUp(50.0f<ms>, 1)
                .KeyUp(50.0f<ms>, 2)
                .KeyUp(50.0f<ms>, 3)

                .KeyDown(980.0f<ms>, 3)
                .KeyDown(995.0f<ms>, 2)
                .KeyDown(1005.0f<ms>, 1)
                .KeyDown(1020.0f<ms>, 0)
                .KeyUp(1050.0f<ms>, 0)
                .KeyUp(1050.0f<ms>, 1)
                .KeyUp(1050.0f<ms>, 2)
                .KeyUp(1050.0f<ms>, 3)
                .Build()

        let event_processing = GameplayEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(8, event_processing.Events.Count)

        Assert.AreEqual(
            event_processing.Events |> Seq.map _.Time |> Seq.sort,
            event_processing.Events |> Seq.map _.Time
        )

        Assert.AreEqual(
            [
                HIT(-20.0f<ms / rate>, false)
                HIT(-5.0f<ms / rate>, false)
                HIT(5.0f<ms / rate>, false)
                HIT(20.0f<ms / rate>, false)

                HIT(-20.0f<ms / rate>, false)
                HIT(-5.0f<ms / rate>, false)
                HIT(5.0f<ms / rate>, false)
                HIT(20.0f<ms / rate>, false)
            ],
            event_processing.Events |> Seq.map _.Action
        )

        Assert.AreEqual(
            [
                0; 1; 2; 3
                3; 2; 1; 0
            ],
            event_processing.Events |> Seq.map _.Column
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
                .KeyDownFor(0.0f<ms>, 30.0f<ms>)
                .KeyDownFor(500.0f<ms>, 30.0f<ms>)
                .KeyDownFor(1000.0f<ms>, 30.0f<ms>)
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
                .KeyDownFor(-1000.0f<ms>, 30.0f<ms>)
                .KeyDownFor(0.0f<ms>, 30.0f<ms>)
                .KeyDownFor(1000.0f<ms>, 30.0f<ms>)
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
                .KeyDownFor(0.0f<ms>, 30.0f<ms>)
                .KeyDownFor(1000.0f<ms>, 30.0f<ms>)
                .KeyDownFor(2000.0f<ms>, 30.0f<ms>)
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
    
    [<Test>]
    let MissedNotes_Basic () =
        let notes = 
            ChartBuilder(4)
                .Note(0.0f<ms>)
                .Note(200.0f<ms>)
                .Note(400.0f<ms>)
                .Build()

        let replay = 
            ReplayBuilder()
                .Build()

        let event_processing = GameplayEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                HIT(180.0f<ms / rate>, true)
                HIT(180.0f<ms / rate>, true)
                HIT(180.0f<ms / rate>, true)
            ],
            event_processing.Events |> Seq.map _.Action
        )

    [<Test>]
    let MissedNotes_AfterHit () =
        let notes = 
            ChartBuilder(4)
                .Note(0.0f<ms>)
                .Note(200.0f<ms>)
                .Note(400.0f<ms>)
                .Build()

        let replay = 
            ReplayBuilder()
                .KeyDown(0.0f<ms>)
                .Build()

        let event_processing = GameplayEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                HIT(0.0f<ms / rate>, false)
                HIT(180.0f<ms / rate>, true)
                HIT(180.0f<ms / rate>, true)
            ],
            event_processing.Events |> Seq.map _.Action
        )

    [<Test>]
    let MissedNotes_BetweenHits () =
        let notes = 
            ChartBuilder(4)
                .Note(0.0f<ms>)
                .Note(200.0f<ms>)
                .Note(400.0f<ms>)
                .Build()

        let replay = 
            ReplayBuilder()
                .KeyDownFor(0.0f<ms>, 30.0f<ms>)
                .KeyDownFor(400.0f<ms>, 30.0f<ms>)
                .Build()

        let event_processing = GameplayEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                HIT(0.0f<ms / rate>, false)
                HIT(180.0f<ms / rate>, true)
                HIT(0.0f<ms / rate>, false)
            ],
            event_processing.Events |> Seq.map _.Action
        )
    
    [<Test>]
    let MissedNotes_TapsOutOfRange () =
        let notes = 
            ChartBuilder(4)
                .Note(0.0f<ms>)
                .Note(1000.0f<ms>)
                .Build()

        let replay = 
            ReplayBuilder()
                .KeyDownFor(-200.0f<ms>, 30.0f<ms>)
                .KeyDownFor(800.0f<ms>, 30.0f<ms>)
                .Build()

        let event_processing = GameplayEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                HIT(180.0f<ms / rate>, true)
                HIT(180.0f<ms / rate>, true)
            ],
            event_processing.Events |> Seq.map _.Action |> Seq.filter ((<>) GHOST_TAP)
        )

    [<Test>]
    let MissedNotes_TapsInOtherColumns () =
        let notes = 
            ChartBuilder(4)
                .Note(0.0f<ms>)
                .Note(1000.0f<ms>)
                .Build()

        let replay = 
            ReplayBuilder()
                .KeyDownFor(-200.0f<ms>, 30.0f<ms>, 1)
                .KeyDownFor(800.0f<ms>, 30.0f<ms>, 2)
                .Build()

        let event_processing = GameplayEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                HIT(180.0f<ms / rate>, true)
                HIT(180.0f<ms / rate>, true)
            ],
            event_processing.Events |> Seq.map _.Action |> Seq.filter ((<>) GHOST_TAP)
        )
    
    [<Test>]
    let HoldNote_Basic () =
        let notes = 
            ChartBuilder(4)
                .Hold(0.0f<ms>, 1000.0f<ms>)
                .Build()

        let replay = 
            ReplayBuilder()
                .KeyDownFor(0.0f<ms>, 1000.0f<ms>)
                .Build()

        let event_processing = GameplayEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                HOLD(0.0f<ms / rate>, false)
                RELEASE(0.0f<ms / rate>, false, false, false, 0.0f<ms/rate>, false)
            ],
            event_processing.Events |> Seq.map _.Action
        )