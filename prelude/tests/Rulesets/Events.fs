namespace Prelude.Tests.Rulesets

open NUnit.Framework
open Prelude
open Prelude.Gameplay.Replays
open Prelude.Gameplay.Rulesets
open Prelude.Gameplay.Scoring

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

    [<Test(Description = "Interlude's behaviour: When a badly hit note is closer than the next note that would be hit, ignore the input")>]
    let TapNotes_EarlyBoundary_InterludeCbrushWindow () =
        let notes =
            ChartBuilder(4)
                .Note(0.0f<ms>)
                .Note(180.0f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownFor(-180.0f<ms>, 30.0f<ms>) // hits note 0
                .KeyDownFor(0.0f<ms>, 30.0f<ms>) // would be +0ms on note 0, which has a -180ms hit. eat the hit (don't have it hit note 1)
                .Build()

        let event_processing = GameplayEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                HIT(-180.0f<ms / rate>, false)
                HIT(180.0f<ms / rate>, true)
            ],
            event_processing.Events |> Seq.map _.Action
        )

    [<Test(Description = "Interlude's behaviour: If a hit is at least +-90ms off, look for something better to hit")>]
    let TapNotes_LateBoundary_InterludeCbrushWindow () =
        let notes =
            ChartBuilder(4)
                .Note(0.0f<ms>)
                .Note(180.0f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownFor(180.0f<ms>, 30.0f<ms>) // would be +180ms on note 0, but +0ms on note 1, so hit note 1
                .KeyDownFor(360.0f<ms>, 30.0f<ms>) // this tap goes nowhere, ghost tap
                .Build()

        let event_processing = GameplayEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                HIT(0.0f<ms / rate>, false)
                HIT(180.0f<ms / rate>, true)
                GHOST_TAP
            ],
            event_processing.Events |> Seq.map _.Action
        )

    [<Test>]
    let TapNotes_EarlyBoundary_OsuMania () =
        let notes =
            ChartBuilder(4)
                .Note(0.0f<ms>)
                .Note(180.0f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownFor(-180.0f<ms>, 30.0f<ms>)
                .KeyDownFor(0.0f<ms>, 30.0f<ms>)
                .Build()

        let ruleset = { RULESET with HitMechanics = { NotePriority = NotePriority.OsuMania; GhostTapJudgement = None } }

        let event_processing = GameplayEventCollector(ruleset, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                HIT(-180.0f<ms / rate>, false)
                HIT(-180.0f<ms / rate>, false)
            ],
            event_processing.Events |> Seq.map _.Action
        )

    [<Test>]
    let TapNotes_LateBoundary_OsuMania () =
        let notes =
            ChartBuilder(4)
                .Note(0.0f<ms>)
                .Note(180.0f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownFor(180.0f<ms>, 30.0f<ms>)
                .KeyDownFor(360.0f<ms>, 30.0f<ms>)
                .Build()

        let ruleset = { RULESET with HitMechanics = { NotePriority = NotePriority.OsuMania; GhostTapJudgement = None } }

        let event_processing = GameplayEventCollector(ruleset, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                HIT(180.0f<ms / rate>, false)
                HIT(180.0f<ms / rate>, false)
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
    let TapNotes_ColumnLock_OsuMania () =
        let notes =
            ChartBuilder(4)
                .Note(0.0f<ms>)
                .Note(100.0f<ms>)
                .Note(200.0f<ms>)
                .Note(300.0f<ms>)
                .Note(400.0f<ms>)
                .Note(500.0f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownFor(-90.0f<ms>, 10.0f<ms>)
                .KeyDownFor(10.0f<ms>, 30.0f<ms>)
                .KeyDownFor(110.0f<ms>, 30.0f<ms>)
                .KeyDownFor(210.0f<ms>, 30.0f<ms>)
                .KeyDownFor(310.0f<ms>, 30.0f<ms>)
                .KeyDownFor(410.0f<ms>, 30.0f<ms>)
                .KeyDownFor(510.0f<ms>, 30.0f<ms>)
                .Build()

        let ruleset = { RULESET with HitMechanics = { NotePriority = NotePriority.OsuMania; GhostTapJudgement = None } }

        let event_processing = GameplayEventCollector(ruleset, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                HIT(-90.0f<ms / rate>, false)
                HIT(-90.0f<ms / rate>, false)
                HIT(-90.0f<ms / rate>, false)
                HIT(-90.0f<ms / rate>, false)
                HIT(-90.0f<ms / rate>, false)
                HIT(-90.0f<ms / rate>, false)
                GHOST_TAP
            ],
            event_processing.Events |> Seq.map _.Action
        )

    [<Test>]
    let TapNotes_ColumnLock_Interlude () =
        let notes =
            ChartBuilder(4)
                .Note(0.0f<ms>)
                .Note(100.0f<ms>)
                .Note(200.0f<ms>)
                .Note(300.0f<ms>)
                .Note(400.0f<ms>)
                .Note(500.0f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownFor(-90.0f<ms>, 30.0f<ms>)
                .KeyDownFor(10.0f<ms>, 30.0f<ms>) // this input gets eaten
                .KeyDownFor(110.0f<ms>, 30.0f<ms>)
                .KeyDownFor(210.0f<ms>, 30.0f<ms>)
                .KeyDownFor(310.0f<ms>, 30.0f<ms>)
                .KeyDownFor(410.0f<ms>, 30.0f<ms>)
                .KeyDownFor(510.0f<ms>, 30.0f<ms>)
                .Build()

        let event_processing = GameplayEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                HIT(-90.0f<ms / rate>, false)
                HIT(10.0f<ms / rate>, false)
                HIT(10.0f<ms / rate>, false)
                HIT(10.0f<ms / rate>, false)
                HIT(10.0f<ms / rate>, false)
                HIT(10.0f<ms / rate>, false)
            ],
            event_processing.Events |> Seq.map _.Action
        )

    [<Test>]
    let TapNotes_ColumnLock_Interlude_HalfRate () =
        let notes =
            ChartBuilder(4)
                .Note(0.0f<ms>)
                .Note(50.0f<ms>)
                .Note(100.0f<ms>)
                .Note(150.0f<ms>)
                .Note(200.0f<ms>)
                .Note(250.0f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownFor(-45.0f<ms>, 15.0f<ms>)
                .KeyDownFor(5.0f<ms>, 15.0f<ms>) // eaten like in `TapNotes_ColumnLock_Interlude`
                .KeyDownFor(55.0f<ms>, 15.0f<ms>)
                .KeyDownFor(105.0f<ms>, 15.0f<ms>)
                .KeyDownFor(155.0f<ms>, 15.0f<ms>)
                .KeyDownFor(205.0f<ms>, 15.0f<ms>)
                .KeyDownFor(255.0f<ms>, 15.0f<ms>)
                .Build()

        let event_processing = GameplayEventCollector(RULESET, 4, replay, notes, 0.5f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                HIT(-90.0f<ms / rate>, false)
                HIT(10.0f<ms / rate>, false)
                HIT(10.0f<ms / rate>, false)
                HIT(10.0f<ms / rate>, false)
                HIT(10.0f<ms / rate>, false)
                HIT(10.0f<ms / rate>, false)
            ],
            event_processing.Events |> Seq.map _.Action
        )

    [<Test>]
    let TapNotes_ColumnLock_Interlude_DoubleRate () =
        let notes =
            ChartBuilder(4)
                .Note(0.0f<ms>)
                .Note(200.0f<ms>)
                .Note(400.0f<ms>)
                .Note(600.0f<ms>)
                .Note(800.0f<ms>)
                .Note(1000.0f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownFor(-180.0f<ms>, 60.0f<ms>)
                .KeyDownFor(20.0f<ms>, 60.0f<ms>) // eaten like in `TapNotes_ColumnLock_Interlude`
                .KeyDownFor(220.0f<ms>, 60.0f<ms>)
                .KeyDownFor(420.0f<ms>, 60.0f<ms>)
                .KeyDownFor(620.0f<ms>, 60.0f<ms>)
                .KeyDownFor(820.0f<ms>, 60.0f<ms>)
                .KeyDownFor(1020.0f<ms>, 60.0f<ms>)
                .Build()

        let event_processing = GameplayEventCollector(RULESET, 4, replay, notes, 2.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                HIT(-90.0f<ms / rate>, false)
                HIT(10.0f<ms / rate>, false)
                HIT(10.0f<ms / rate>, false)
                HIT(10.0f<ms / rate>, false)
                HIT(10.0f<ms / rate>, false)
                HIT(10.0f<ms / rate>, false)
            ],
            event_processing.Events |> Seq.map _.Action
        )

    [<Test>]
    let TapNotes_ColumnLock_Interlude_Threshold () =
        let notes =
            ChartBuilder(4)
                .Note(0.0f<ms>)
                .Note(100.0f<ms>)
                .Note(200.0f<ms>)
                .Note(300.0f<ms>)
                .Note(400.0f<ms>)
                .Note(500.0f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownFor(-89.0f<ms>, 10.0f<ms>)
                .KeyDownFor(11.0f<ms>, 30.0f<ms>) // this input does not get eaten
                .KeyDownFor(111.0f<ms>, 30.0f<ms>)
                .KeyDownFor(211.0f<ms>, 30.0f<ms>)
                .KeyDownFor(311.0f<ms>, 30.0f<ms>)
                .KeyDownFor(411.0f<ms>, 30.0f<ms>)
                .KeyDownFor(511.0f<ms>, 30.0f<ms>)
                .Build()

        let event_processing = GameplayEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                HIT(-89.0f<ms / rate>, false)
                HIT(-89.0f<ms / rate>, false)
                HIT(-89.0f<ms / rate>, false)
                HIT(-89.0f<ms / rate>, false)
                HIT(-89.0f<ms / rate>, false)
                HIT(-89.0f<ms / rate>, false)
                GHOST_TAP
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

    [<Test>]
    let HoldNote_Basic_Offset () =
        let notes =
            ChartBuilder(4)
                .Hold(0.0f<ms>, 1000.0f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownUntil(10.0f<ms>, 990.0f<ms>)
                .Build()

        let event_processing = GameplayEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                HOLD(10.0f<ms / rate>, false)
                RELEASE(-10.0f<ms / rate>, false, false, false, 10.0f<ms/rate>, false)
            ],
            event_processing.Events |> Seq.map _.Action
        )

    [<Test>]
    let HoldNote_InnerBoundaries () =
        let notes =
            ChartBuilder(4)
                .Hold(0.0f<ms>, 1000.0f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownUntil(180.0f<ms>, 820.0f<ms>)
                .Build()

        let event_processing = GameplayEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                HOLD(180.0f<ms / rate>, false)
                RELEASE(-180.0f<ms / rate>, false, false, false, 180.0f<ms/rate>, false)
            ],
            event_processing.Events |> Seq.map _.Action
        )

    [<Test>]
    let HoldNote_OuterBoundaries () =
        let notes =
            ChartBuilder(4)
                .Hold(0.0f<ms>, 1000.0f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownUntil(-180.0f<ms>, 1180.0f<ms>)
                .Build()

        let event_processing = GameplayEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                HOLD(-180.0f<ms / rate>, false)
                RELEASE(180.0f<ms / rate>, false, false, false, -180.0f<ms/rate>, false)
            ],
            event_processing.Events |> Seq.map _.Action
        )

    [<Test>]
    let HoldNote_InnerBoundaries_HalfRate () =
        let notes =
            ChartBuilder(4)
                .Hold(0.0f<ms>, 1000.0f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownUntil(90.0f<ms>, 910.0f<ms>)
                .Build()

        let event_processing = GameplayEventCollector(RULESET, 4, replay, notes, 0.5f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                HOLD(180.0f<ms / rate>, false)
                RELEASE(-180.0f<ms / rate>, false, false, false, 180.0f<ms/rate>, false)
            ],
            event_processing.Events |> Seq.map _.Action
        )

    [<Test>]
    let HoldNote_OuterBoundaries_HalfRate () =
        let notes =
            ChartBuilder(4)
                .Hold(0.0f<ms>, 1000.0f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownUntil(-90.0f<ms>, 1090.0f<ms>)
                .Build()

        let event_processing = GameplayEventCollector(RULESET, 4, replay, notes, 0.5f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                HOLD(-180.0f<ms / rate>, false)
                RELEASE(180.0f<ms / rate>, false, false, false, -180.0f<ms/rate>, false)
            ],
            event_processing.Events |> Seq.map _.Action
        )

    [<Test>]
    let HoldNote_InnerBoundaries_DoubleRate () =
        let notes =
            ChartBuilder(4)
                .Hold(0.0f<ms>, 1000.0f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownUntil(360.0f<ms>, 640.0f<ms>)
                .Build()

        let event_processing = GameplayEventCollector(RULESET, 4, replay, notes, 2.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                HOLD(180.0f<ms / rate>, false)
                RELEASE(-180.0f<ms / rate>, false, false, false, 180.0f<ms/rate>, false)
            ],
            event_processing.Events |> Seq.map _.Action
        )

    [<Test>]
    let HoldNote_OuterBoundaries_DoubleRate () =
        let notes =
            ChartBuilder(4)
                .Hold(0.0f<ms>, 1000.0f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownUntil(-360.0f<ms>, 1360.0f<ms>)
                .Build()

        let event_processing = GameplayEventCollector(RULESET, 4, replay, notes, 2.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                HOLD(-180.0f<ms / rate>, false)
                RELEASE(180.0f<ms / rate>, false, false, false, -180.0f<ms/rate>, false)
            ],
            event_processing.Events |> Seq.map _.Action
        )

    [<Test>]
    let HoldNote_Missed () =
        let notes =
            ChartBuilder(4)
                .Hold(0.0f<ms>, 1000.0f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .Build()

        let event_processing = GameplayEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                HOLD(180.0f<ms / rate>, true)
                RELEASE(180.0f<ms / rate>, true, false, true, 180.0f<ms/rate>, true)
            ],
            event_processing.Events |> Seq.map _.Action
        )

    [<Test>]
    let HoldNote_Overheld () =
        let notes =
            ChartBuilder(4)
                .Hold(0.0f<ms>, 1000.0f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownFor(0.0f<ms>, 2000.0f<ms>)
                .Build()

        let event_processing = GameplayEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                HOLD(0.0f<ms / rate>, false)
                RELEASE(180.0f<ms / rate>, true, true, false, 0.0f<ms/rate>, false)
            ],
            event_processing.Events |> Seq.map _.Action
        )

    [<Test>]
    let HoldNote_Dropped () =
        let notes =
            ChartBuilder(4)
                .Hold(0.0f<ms>, 1000.0f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownFor(0.0f<ms>, 500.0f<ms>)
                .Build()

        let event_processing = GameplayEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                HOLD(0.0f<ms / rate>, false)
                DROP_HOLD
                RELEASE(180.0f<ms / rate>, true, false, true, 0.0f<ms/rate>, false)
            ],
            event_processing.Events |> Seq.map _.Action
        )

    [<Test>]
    let HoldNote_Regrabbed () =
        let notes =
            ChartBuilder(4)
                .Hold(0.0f<ms>, 1000.0f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownFor(0.0f<ms>, 400.0f<ms>)
                .KeyDownFor(500.0f<ms>, 500.0f<ms>)
                .Build()

        let event_processing = GameplayEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                HOLD(0.0f<ms / rate>, false)
                DROP_HOLD
                REGRAB_HOLD
                RELEASE(0.0f<ms / rate>, false, false, true, 0.0f<ms/rate>, false)
            ],
            event_processing.Events |> Seq.map _.Action
        )

    [<Test>]
    let HoldNote_Multiple_Regrabs () =
        let notes =
            ChartBuilder(4)
                .Hold(0.0f<ms>, 1000.0f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownFor(0.0f<ms>, 300.0f<ms>)
                .KeyDownFor(350.0f<ms>, 100.0f<ms>)
                .KeyDownFor(500.0f<ms>, 100.0f<ms>)
                .KeyDownFor(650.0f<ms>, 350.0f<ms>)
                .Build()

        let event_processing = GameplayEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            event_processing.Events |> Seq.map _.Time |> Seq.sort,
            event_processing.Events |> Seq.map _.Time
        )

        Assert.AreEqual(
            [
                HOLD(0.0f<ms / rate>, false)
                DROP_HOLD
                REGRAB_HOLD
                DROP_HOLD
                REGRAB_HOLD
                DROP_HOLD
                REGRAB_HOLD
                RELEASE(0.0f<ms / rate>, false, false, true, 0.0f<ms/rate>, false)
            ],
            event_processing.Events |> Seq.map _.Action
        )

    [<Test>]
    let HoldNote_Regrabbed_Overheld () =
        let notes =
            ChartBuilder(4)
                .Hold(0.0f<ms>, 1000.0f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownFor(0.0f<ms>, 400.0f<ms>)
                .KeyDownFor(500.0f<ms>, 1000.0f<ms>)
                .Build()

        let event_processing = GameplayEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                HOLD(0.0f<ms / rate>, false)
                DROP_HOLD
                REGRAB_HOLD
                RELEASE(180.0f<ms / rate>, true, true, true, 0.0f<ms/rate>, false)
            ],
            event_processing.Events |> Seq.map _.Action
        )

    [<Test>]
    let HoldNote_MissedHead_Regrabbed () =
        let notes =
            ChartBuilder(4)
                .Hold(0.0f<ms>, 1000.0f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownFor(500.0f<ms>, 500.0f<ms>)
                .Build()

        let event_processing = GameplayEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                HOLD(180.0f<ms / rate>, true)
                REGRAB_HOLD
                RELEASE(0.0f<ms / rate>, false, false, true, 180.0f<ms/rate>, true)
            ],
            event_processing.Events |> Seq.map _.Action
        )

    [<Test>]
    let HoldNote_MissedHead_HeldEarly () =
        let notes =
            ChartBuilder(4)
                .Hold(0.0f<ms>, 1000.0f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownFor(-500.0f<ms>, 1500.0f<ms>)
                .Build()

        let event_processing = GameplayEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                HOLD(180.0f<ms / rate>, true)
                RELEASE(180.0f<ms / rate>, true, false, true, 180.0f<ms/rate>, true)
            ],
            event_processing.Events |> Seq.map _.Action
        )

    [<Test>]
    let HoldNote_Dropped_IntoNote () =
        let notes =
            ChartBuilder(4)
                .Hold(0.0f<ms>, 999.0f<ms>)
                .Note(1000.0f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownFor(0.0f<ms>, 500.0f<ms>)
                .KeyDownFor(990.0f<ms>, 10.0f<ms>)
                .Build()

        let event_processing = GameplayEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                HOLD(0.0f<ms / rate>, false)
                DROP_HOLD
                RELEASE(180.0f<ms / rate>, true, false, true, 0.0f<ms/rate>, false)
                HIT(-10.0f<ms / rate>, false)
            ],
            event_processing.Events |> Seq.map _.Action
        )

    [<Test>]
    let HoldNote_Dropped_IntoHoldNote () =
        let notes =
            ChartBuilder(4)
                .Hold(0.0f<ms>, 999.0f<ms>)
                .Hold(1000.0f<ms>, 1010.0f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownFor(0.0f<ms>, 500.0f<ms>)
                .KeyDownFor(990.0f<ms>, 30.0f<ms>)
                .Build()

        let event_processing = GameplayEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                HOLD(0.0f<ms / rate>, false)
                DROP_HOLD
                RELEASE(180.0f<ms / rate>, true, false, true, 0.0f<ms/rate>, false)
                HOLD(-10.0f<ms / rate>, false)
                RELEASE(10.0f<ms / rate>, false, false, false, -10.0f<ms/rate>, false)
            ],
            event_processing.Events |> Seq.map _.Action
        )

    [<Test>]
    let HoldNote_VeryLateRegrab () =
        let notes =
            ChartBuilder(4)
                .Hold(0.0f<ms>, 1000.0f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownFor(0.0f<ms>, 500.0f<ms>)
                .KeyDownFor(1179.0f<ms>, 1.0f<ms>)
                .Build()

        let event_processing = GameplayEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                HOLD(0.0f<ms / rate>, false)
                DROP_HOLD
                REGRAB_HOLD
                RELEASE(180.0f<ms / rate>, false, false, true, 0.0f<ms/rate>, false)
            ],
            event_processing.Events |> Seq.map _.Action
        )

    [<Test>]
    let TimeStepping_Normal () =
        let notes =
            ChartBuilder(4)
                .Note(0.0f<ms>)
                .Hold(500.0f<ms>, 1000.0f<ms>)
                .Note(1500.0f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownFor(500.0f<ms>, 200.0f<ms>)
                .KeyDownFor(800.0f<ms>, 200.0f<ms>)
                .KeyDownFor(1480.0f<ms>, 20.0f<ms>)
                .Build()

        let event_step_processing = GameplayEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)

        let mutable prev_expected = 0
        let step time expected_event_count =
            printfn "STEPPING TO %.1fms (expecting %i new event(s)):" time (expected_event_count - prev_expected)
            prev_expected <- expected_event_count
            event_step_processing.Update time
            Assert.AreEqual(expected_event_count, event_step_processing.Events.Count)

        step -1000.0f<ms> 0
        step -1.0f<ms> 0
        step 0.0f<ms> 0
        step 179.9f<ms> 0
        step 180.0f<ms> 0
        step 180.1f<ms> 1
        step 499.9f<ms> 1
        step 500.0f<ms> 2
        step 699.9f<ms> 2
        step 700.0f<ms> 3
        step 799.9f<ms> 3
        step 800.0f<ms> 4
        step 999.9f<ms> 4
        step 1000.0f<ms> 5
        step 1480.0f<ms> 6
        step 1500.0f<ms> 6
        step 5000.0f<ms> 6
        step Time.infinity 6

        printfn "Now stepping all at once:"

        let rewind_replay = replay.GetFullReplay() |> StoredReplayProvider

        let event_processing = GameplayEventCollector(RULESET, 4, rewind_replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            event_step_processing.Events,
            event_processing.Events
        )

    [<Test(Description = "Passing in decreasing timestamps should perhaps crash in future, currently parts of the client rely on it not doing anything")>]
    let TimeStepping_BackwardsHasNoEffect () =
        let notes =
            ChartBuilder(4)
                .Note(0.0f<ms>)
                .Hold(500.0f<ms>, 1000.0f<ms>)
                .Note(1500.0f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownFor(500.0f<ms>, 200.0f<ms>)
                .KeyDownFor(800.0f<ms>, 200.0f<ms>)
                .KeyDownFor(1480.0f<ms>, 20.0f<ms>)
                .Build()

        let event_step_processing = GameplayEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)

        let mutable prev_expected = 0
        let step time expected_event_count =
            printfn "STEPPING TO %.1fms (expecting %i new event(s)):" time (expected_event_count - prev_expected)
            prev_expected <- expected_event_count
            event_step_processing.Update time
            Assert.AreEqual(expected_event_count, event_step_processing.Events.Count)

        step -1000.0f<ms> 0
        step -1.0f<ms> 0
        step 0.0f<ms> 0
        step 179.9f<ms> 0
        step 180.0f<ms> 0
        step 180.1f<ms> 1
        step 499.9f<ms> 1
        step 500.0f<ms> 2
        step 699.9f<ms> 2
        step 700.0f<ms> 3

        step 500.0f<ms> 3
        step 180.0f<ms> 3
        step 0.0f<ms> 3
        step -180.0f<ms> 3
        step -5000.0f<ms> 3
        step -Time.infinity 3

        step 799.9f<ms> 3
        step 800.0f<ms> 4
        step 999.9f<ms> 4
        step 1000.0f<ms> 5
        step 1480.0f<ms> 6
        step 1500.0f<ms> 6
        step 5000.0f<ms> 6
        step Time.infinity 6

        printfn "Now stepping all at once:"

        let rewind_replay = replay.GetFullReplay() |> StoredReplayProvider

        let event_processing = GameplayEventCollector(RULESET, 4, rewind_replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            event_step_processing.Events,
            event_processing.Events
        )

    [<Test(Description = "Interlude's cbrush window mechanic means you can hit notes in reverse order. Documented so it isn't reported as a bug")>]
    let SCJ4_BackwardsNotes () =
        let notes =
            ChartBuilder(4)
                .Note(0.0f<ms>)
                .Note(100.0f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownFor(100.0f<ms>, 1.0f<ms>)
                .KeyDownFor(102.0f<ms>, 30.0f<ms>)
                .Build()

        let event_processing = GameplayEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                HIT(0.0f<ms / rate>, false)
                HIT(102.0f<ms / rate>, false)
            ],
            event_processing.Events |> Seq.map _.Action
        )

        Assert.AreEqual(
            event_processing.Events |> Seq.map (_.Time),
            event_processing.Events |> Seq.map (_.Time) |> Seq.sort
        )

    [<Test(Description = "It is impossible to hit notes backwards in osu!mania (stable)")>]
    let OsuOd8_NoBackwardsNotes () =
        let notes =
            ChartBuilder(4)
                .Note(0.0f<ms>)
                .Note(100.0f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownFor(100.0f<ms>, 1.0f<ms>)
                .KeyDownFor(102.0f<ms>, 30.0f<ms>)
                .Build()

        let event_processing = GameplayEventCollector(OsuMania.create 8.0f OsuMania.NoMod, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                HIT(100.0f<ms / rate>, false)
                HIT(2.0f<ms / rate>, false)
            ],
            event_processing.Events |> Seq.map _.Action
        )

        Assert.AreEqual(
            event_processing.Events |> Seq.map (_.Time),
            event_processing.Events |> Seq.map (_.Time) |> Seq.sort
        )

    [<Test(Description = "Etterna uses nearest note, it is known that you can hit notes backwards")>]
    let Wife3_BackwardsNotes () =
        let notes =
            ChartBuilder(4)
                .Note(0.0f<ms>)
                .Note(100.0f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownFor(51.0f<ms>, 1.0f<ms>)
                .KeyDownFor(53.0f<ms>, 30.0f<ms>)
                .Build()

        let event_processing = GameplayEventCollector(Wife3.create 4, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                HIT(-49.0f<ms / rate>, false)
                HIT(53.0f<ms / rate>, false)
            ],
            event_processing.Events |> Seq.map _.Action
        )

        Assert.AreEqual(
            event_processing.Events |> Seq.map (_.Time),
            event_processing.Events |> Seq.map (_.Time) |> Seq.sort
        )

    [<Test>]
    let Wife3_TouchingWindows () =
        let notes =
            ChartBuilder(4)
                .Note(0.0f<ms>)
                .Note(360.0f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownFor(180.0f<ms>, 30.0f<ms>)
                .Build()

        let event_processing = GameplayEventCollector(Wife3.create 4, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                HIT(180.0f<ms / rate>, false)
                HIT(180.0f<ms / rate>, true)
            ],
            event_processing.Events |> Seq.map _.Action
        )

        Assert.AreEqual(
            event_processing.Events |> Seq.map (_.Time),
            event_processing.Events |> Seq.map (_.Time) |> Seq.sort
        )

    [<Test>]
    let SCJ4_TouchingWindows () =
        let notes =
            ChartBuilder(4)
                .Note(0.0f<ms>)
                .Note(360.0f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownFor(180.0f<ms>, 30.0f<ms>)
                .Build()

        let event_processing = GameplayEventCollector(Wife3.create 4, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                HIT(180.0f<ms / rate>, false)
                HIT(180.0f<ms / rate>, true)
            ],
            event_processing.Events |> Seq.map _.Action
        )

        Assert.AreEqual(
            event_processing.Events |> Seq.map (_.Time),
            event_processing.Events |> Seq.map (_.Time) |> Seq.sort
        )

    [<Test>]
    let ExpiredNoteMarkedBeforeTapProcessed () =
        let notes =
            ChartBuilder(4)
                .Note(0.0f<ms>)
                .Note(360.5f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownFor(180.5f<ms>, 30.0f<ms>)
                .Build()

        let event_processing = GameplayEventCollector(Wife3.create 4, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                0, HIT(180.0f<ms / rate>, true)
                1, HIT(-180.0f<ms / rate>, false)
            ],
            event_processing.Events |> Seq.map (fun e -> e.Index, e.Action)
        )

        Assert.AreEqual(
            event_processing.Events |> Seq.map (_.Time),
            event_processing.Events |> Seq.map (_.Time) |> Seq.sort
        )

    let SIMPLE_RULESET : Ruleset =
        {
            Name = "SIMPLE"
            Description = "SIMPLE"
            Judgements =
                [|
                    {
                        Name = "HIT"
                        Color = Color.White
                        TimingWindows = Some (-180.0f<ms / rate>, 90.0f<ms / rate>)
                        BreaksCombo = false
                    }
                    {
                        Name = "MISS"
                        Color = Color.Red
                        TimingWindows = None
                        BreaksCombo = true
                    }
                |]
            Grades = [||]
            Lamps = [||]
            HitMechanics = { NotePriority = NotePriority.Etterna; GhostTapJudgement = None }
            HoldMechanics = HoldMechanics.OnlyRequireHold 90.0f<ms / rate>
            Accuracy = AccuracyPoints.PointsPerJudgement [|1.0; 0.0|]
            Formatting = { DecimalPlaces = DecimalPlaces.TWO }
        }

    [<Test>]
    let ExpiredNoteMarkedBeforeTapProcessed_AsymmetricalWindows () =
        let notes =
            ChartBuilder(4)
                .Note(0.0f<ms>)
                .Note(90.5f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownFor(90.5f<ms>, 30.0f<ms>)
                .Build()

        let event_processing = GameplayEventCollector(SIMPLE_RULESET, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                0, HIT(90.0f<ms / rate>, true)
                1, HIT(0.0f<ms / rate>, false)
            ],
            event_processing.Events |> Seq.map (fun e -> e.Index, e.Action)
        )

        Assert.AreEqual(
            event_processing.Events |> Seq.map (_.Time),
            event_processing.Events |> Seq.map (_.Time) |> Seq.sort
        )

    [<Test>]
    let ExpiredNoteMarkedBeforeTapProcessed_WiderWindowsDueToReleases () =
        let notes =
            ChartBuilder(4)
                .Note(0.0f<ms>)
                .Note(90.5f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownFor(90.5f<ms>, 30.0f<ms>)
                .Build()

        let ruleset =
            { SIMPLE_RULESET with
                HoldMechanics = HoldMechanics.OnlyRequireHold 180.0f<ms / rate>
            }

        let event_processing = GameplayEventCollector(ruleset, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                1, HIT(0.0f<ms / rate>, false)
                0, HIT(90.0f<ms / rate>, true)
            ],
            event_processing.Events |> Seq.map (fun e -> e.Index, e.Action)
        )

        Assert.AreEqual(
            event_processing.Events |> Seq.map (_.Time),
            event_processing.Events |> Seq.map (_.Time) |> Seq.sort
        )

    [<Test>]
    let FirstNoteIndependence () =
        let replay =
            ReplayBuilder()
                .KeyDownFor(22.5f<ms>, 30.0f<ms>)
                .KeyDownFor(90.0f<ms>, 30.0f<ms>)
                .KeyDownFor(200.0f<ms>, 30.0f<ms>)
                .Build()

        printfn "FIRST NOTE AT 0ms"

        let notes =
            ChartBuilder(4)
                .Note(0.0f<ms>)
                .Note(100.0f<ms>)
                .Note(200.0f<ms>)
                .Build()

        let event_processing = GameplayEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        printfn "FIRST NOTE AT 1000ms"

        let notes_2 =
            ChartBuilder(4)
                .Note(1000.0f<ms>)
                .Note(1100.0f<ms>)
                .Note(1200.0f<ms>)
                .Build()

        let event_processing_2 = GameplayEventCollector(RULESET, 4, StoredReplayProvider(replay.GetFullReplay()), notes_2, 1.0f<rate>)
        event_processing_2.Update Time.infinity

        Assert.AreEqual(event_processing.Events, event_processing_2.Events)

    [<Test>]
    let FirstNoteIndependence_IgnoreNotesBefore () =
        let replay =
            ReplayBuilder()
                .KeyDownFor(22.5f<ms>, 30.0f<ms>)
                .KeyDownFor(90.0f<ms>, 30.0f<ms>)
                .KeyDownFor(200.0f<ms>, 30.0f<ms>)
                .Build()

        printfn "FIRST NOTE AT 0ms"

        let notes =
            ChartBuilder(4)
                .Note(0.0f<ms>)
                .Note(100.0f<ms>)
                .Note(200.0f<ms>)
                .Build()

        let event_processing = GameplayEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
        event_processing.IgnoreNotesBefore 100.0f<ms>
        event_processing.Update Time.infinity

        printfn "FIRST NOTE AT 1000ms"

        let notes_2 =
            ChartBuilder(4)
                .Note(1000.0f<ms>)
                .Note(1100.0f<ms>)
                .Note(1200.0f<ms>)
                .Build()

        let event_processing_2 = GameplayEventCollector(RULESET, 4, StoredReplayProvider(replay.GetFullReplay()), notes_2, 1.0f<rate>)
        event_processing_2.IgnoreNotesBefore 1100.0f<ms>
        event_processing_2.Update Time.infinity

        Assert.AreEqual(event_processing.Events, event_processing_2.Events)

    [<Test>]
    let IgnoreNotesBefore_NoPartialHolds () =
        let replay =
            ReplayBuilder()
                .KeyDownFor(0.0f<ms>, 1000.0f<ms>)
                .KeyDownFor(2000.0f<ms>, 1000.0f<ms>)
                .Build()

        let notes =
            ChartBuilder(4)
                .Hold(0.0f<ms>, 1000.0f<ms>)
                .Hold(2000.0f<ms>, 3000.0f<ms>)
                .Build()

        let event_processing = GameplayEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
        event_processing.IgnoreNotesBefore 500.0f<ms>
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                HOLD(0.0f<ms / rate>, false)
                RELEASE(0.0f<ms / rate>, false, false, false, 0.0f<ms / rate>, false)
            ],
            event_processing.Events |> Seq.map (fun e -> e.Action)
        )

    [<Test>]
    let IgnoreNotesBefore_NoPartialHolds2 () =
        let notes =
            ChartBuilder(4)
                .Hold(0.0f<ms>, 90.0f<ms>)
                .Hold(100.0f<ms>, 190.0f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownUntil(-1.0f<ms>, 91.0f<ms>)
                .KeyDownUntil(101.0f<ms>, 189.0f<ms>)
                .Build()

        let event_processing = GameplayEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
        event_processing.IgnoreNotesBefore 75.0f<ms>
        event_processing.Update Time.infinity

        printfn "%A" (event_processing.Events |> Seq.map (fun e -> e.Action))

        Assert.AreEqual(
            [
                HOLD(1.0f<ms / rate>, false)
                RELEASE(-1.0f<ms / rate>, false, false, false, 1.0f<ms / rate>, false)
            ],
            event_processing.Events |> Seq.map (fun e -> e.Action)
        )