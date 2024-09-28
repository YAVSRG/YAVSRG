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

        let ruleset = { RULESET with HitMechanics = HitMechanics.OsuMania }

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

        let ruleset = { RULESET with HitMechanics = HitMechanics.OsuMania }

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

        let ruleset = { RULESET with HitMechanics = HitMechanics.OsuMania }

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
                .KeyDownFor(-90.0f<ms>, 10.0f<ms>)
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