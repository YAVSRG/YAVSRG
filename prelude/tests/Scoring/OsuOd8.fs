namespace Prelude.Tests.Scoring

open NUnit.Framework
open Prelude
open Prelude.Gameplay
open Prelude.Gameplay.Rulesets
open Helpers

module ``osu OD8`` =

    let RULESET = ``osu!``.create 8.0f ``osu!``.NoMod

    [<Test>]
    let TapNote () =
        let notes = ChartBuilder(4).Note(0.0f<ms>).Build()
        let replay = ReplayBuilder().KeyDown(0.0f<ms>, 0).Build()
        let scoring = ScoreProcessor.run RULESET 4 replay notes 1.0f
        let result = scoring.State

        printfn "%A" result

        Assert.AreEqual(1, result.BestCombo)
        Assert.AreEqual(1, result.CurrentCombo)
        Assert.AreEqual(0, result.ComboBreaks)
        Assert.AreEqual(1, result.Judgements.[0])

    [<Test>]
    let HoldNote () =
        let notes = ChartBuilder(4).Hold(0.0f<ms>, 1000.0f<ms>).Build()
        let replay = ReplayBuilder().KeyDown(0.0f<ms>, 0).KeyUp(1000.0f<ms>, 0).Build()
        let scoring = ScoreProcessor.run RULESET 4 replay notes 1.0f
        let result = scoring.State

        printfn "%A" result

        Assert.AreEqual(1, result.BestCombo)
        Assert.AreEqual(1, result.CurrentCombo)
        Assert.AreEqual(0, result.ComboBreaks)
        Assert.AreEqual(1, result.Judgements.[0])

    [<Test>]
    let HoldNote_Overhold () =
        let notes = ChartBuilder(4).Hold(0.0f<ms>, 1000.0f<ms>).Build()
        let replay = ReplayBuilder().KeyDown(0.0f<ms>, 0).Build()
        let scoring = ScoreProcessor.run RULESET 4 replay notes 1.0f
        let result = scoring.State

        printfn "%A" result

        Assert.AreEqual(1, result.BestCombo)
        Assert.AreEqual(1, result.CurrentCombo)
        Assert.AreEqual(0, result.ComboBreaks)
        Assert.AreEqual(1, result.Judgements.[2])

    [<Test>]
    let HoldNote_Underhold () =
        let notes = ChartBuilder(4).Hold(0.0f<ms>, 1000.0f<ms>).Build()
        let replay = ReplayBuilder().KeyDown(0.0f<ms>, 0).KeyUp(500.0f<ms>, 0).Build()
        let scoring = ScoreProcessor.run RULESET 4 replay notes 1.0f
        let result = scoring.State

        printfn "%A" result

        Assert.AreEqual(0, result.BestCombo)
        Assert.AreEqual(0, result.CurrentCombo)
        Assert.GreaterOrEqual(result.ComboBreaks, 1)
        Assert.AreEqual(1, result.Judgements.[5])

    [<Test>]
    let HoldNote_Break_Regrab () =
        let notes = ChartBuilder(4).Hold(0.0f<ms>, 1000.0f<ms>).Build()

        let replay =
            ReplayBuilder()
                .KeyDown(0.0f<ms>, 0)
                .KeyUp(450.0f<ms>, 0)
                .KeyDown(500.0f<ms>, 0)
                .KeyUp(1000.0f<ms>, 0)
                .Build()

        let scoring = ScoreProcessor.run RULESET 4 replay notes 1.0f
        let result = scoring.State

        printfn "%A" result

        Assert.AreEqual(1, result.BestCombo)
        Assert.AreEqual(1, result.CurrentCombo)
        Assert.AreEqual(1, result.ComboBreaks)
        Assert.AreEqual(1, result.Judgements.[4])

    [<Test>]
    let HoldNote_Miss_Regrab () =
        let notes = ChartBuilder(4).Hold(0.0f<ms>, 1000.0f<ms>).Build()
        let replay = ReplayBuilder().KeyDown(500.0f<ms>, 0).KeyUp(1000.0f<ms>, 0).Build()
        let scoring = ScoreProcessor.run RULESET 4 replay notes 1.0f
        let result = scoring.State

        printfn "%A" result

        Assert.AreEqual(0, result.BestCombo)
        Assert.AreEqual(0, result.CurrentCombo)
        Assert.AreEqual(1, result.ComboBreaks)
        Assert.AreEqual(1, result.Judgements.[4])

    [<Test>]
    let HoldNote_EarlyHold_Release () =
        let notes = ChartBuilder(4).Note(0.0f<ms>).Hold(500.0f<ms>, 1500.0f<ms>).Build()
        let replay = ReplayBuilder().KeyDown(0.0f<ms>, 0).KeyUp(1500.0f<ms>, 0).Build()
        let scoring = ScoreProcessor.run RULESET 4 replay notes 1.0f
        let result = scoring.State

        printfn "%A" result

        Assert.AreEqual(1, result.BestCombo)
        Assert.AreEqual(0, result.CurrentCombo)
        Assert.AreEqual(2, result.MaxPossibleCombo)
        Assert.AreEqual(1, result.ComboBreaks)
        Assert.AreEqual(1, result.Judgements.[0])
        Assert.AreEqual(1, result.Judgements.[5])

    [<Test>]
    let HoldNote_Miss () =
        let notes = ChartBuilder(4).Hold(0.0f<ms>, 1000.0f<ms>).Build()
        let replay = ReplayBuilder().Build()
        let scoring = ScoreProcessor.run RULESET 4 replay notes 1.0f
        let result = scoring.State

        printfn "%A" result

        Assert.AreEqual(0, result.BestCombo)
        Assert.AreEqual(0, result.CurrentCombo)
        Assert.AreEqual(1, result.ComboBreaks)
        Assert.AreEqual(1, result.Judgements.[5])
    
    [<Test>]
    [<Ignore("Got a WIP branch to fix these issues sometime soon")>]
    let CbrushHitEating_ControlScenario () =
        let notes = ChartBuilder(4).Note(0.0f<ms>).Note(10.0f<ms>).Note(20.0f<ms>).Build()
        let replay = 
            ReplayBuilder()
                .KeyDown(0.0f<ms>, 0).KeyUp(5.0f<ms>, 0)
                .KeyDown(10.0f<ms>, 0).KeyUp(15.0f<ms>, 0)
                .KeyDown(20.0f<ms>, 0).KeyUp(25.0f<ms>, 0)
                .Build()
        let scoring = ScoreProcessor.run RULESET 4 replay notes 1.0f
        let result = scoring.State

        printfn "%A" result

        Assert.AreEqual(3, result.Judgements.[0])
        Assert.AreEqual(3, result.CurrentCombo)
        Assert.AreEqual(0, result.ComboBreaks)
    
    [<Test>]
    [<Ignore("Got a WIP branch to fix these issues sometime soon")>]
    let CbrushHitEating_EdgeScenario_NoEffect () =
        let notes = ChartBuilder(4).Note(0.0f<ms>).Note(10.0f<ms>).Note(20.0f<ms>).Build()
        let replay = 
            ReplayBuilder()
                .KeyDown(-10.0f<ms>, 0).KeyUp(-5.0f<ms>, 0)
                .KeyDown(0.0f<ms>, 0).KeyUp(5.0f<ms>, 0)
                .KeyDown(10.0f<ms>, 0).KeyUp(15.0f<ms>, 0)
                .Build()
        let scoring = ScoreProcessor.run RULESET 4 replay notes 1.0f
        let result = scoring.State

        printfn "%A" result

        Assert.AreEqual(3, result.Judgements.[0])
        Assert.AreEqual(3, result.CurrentCombo)
        Assert.AreEqual(0, result.ComboBreaks)
    
    [<Test>]
    [<Ignore("Got a WIP branch to fix these issues sometime soon")>]
    let CbrushHitEating_EdgeScenario_HasEffect () =
        let notes = ChartBuilder(4).Note(0.0f<ms>).Note(10.0f<ms>).Note(20.0f<ms>).Build()
        let replay = 
            ReplayBuilder()
                .KeyDown(-10.0f<ms>, 0).KeyUp(-5.0f<ms>, 0)
                .KeyDown(-1.0f<ms>, 0).KeyUp(5.0f<ms>, 0)
                .KeyDown(10.0f<ms>, 0).KeyUp(15.0f<ms>, 0)
                .Build()
        let scoring = ScoreProcessor.run RULESET 4 replay notes 1.0f
        let result = scoring.State

        printfn "%A" result

        Assert.AreNotEqual(3, result.Judgements.[0])
        Assert.AreEqual(1, result.Judgements.[5])

    [<Test>]
    [<Ignore("Got a WIP branch to fix these issues sometime soon")>]
    let CbrushHitEating_ExpectedScenario_HasEffect () =
        let notes = ChartBuilder(4).Note(10.0f<ms>).Note(20.0f<ms>).Note(30.0f<ms>).Build()
        let replay = 
            ReplayBuilder()
                .KeyDown(-10.0f<ms>, 0).KeyUp(-5.0f<ms>, 0)
                .KeyDown(-5.0f<ms>, 0).KeyUp(5.0f<ms>, 0)
                .KeyDown(5.0f<ms>, 0).KeyUp(15.0f<ms>, 0)
                .Build()
        let scoring = ScoreProcessor.run RULESET 4 replay notes 1.0f
        let result = scoring.State

        printfn "%A" result

        Assert.AreNotEqual(3, result.Judgements.[0])
        Assert.AreEqual(1, result.Judgements.[5])

    [<Test>]
    [<Ignore("Got a WIP branch to fix these issues sometime soon")>]
    let WindowsDoNotExtendPastNextObject () =
        let notes = 
            ChartBuilder(4)
                .Note(0.0f<ms>)
                .Note(100.0f<ms>)
                .Note(200.0f<ms>)
                .Note(300.0f<ms>)
                .Note(400.0f<ms>)
                .Note(500.0f<ms>)
                .Note(600.0f<ms>)
                .Build()
        let replay = 
            ReplayBuilder()
                .KeyDown(110.0f<ms>, 0).KeyUp(111.0f<ms>, 0)
                .KeyDown(210.0f<ms>, 0).KeyUp(211.0f<ms>, 0)
                .KeyDown(310.0f<ms>, 0).KeyUp(311.0f<ms>, 0)
                .KeyDown(410.0f<ms>, 0).KeyUp(411.0f<ms>, 0)
                .KeyDown(510.0f<ms>, 0).KeyUp(511.0f<ms>, 0)
                .KeyDown(610.0f<ms>, 0).KeyUp(611.0f<ms>, 0)
                .Build()
        let scoring = ScoreProcessor.run RULESET 4 replay notes 1.0f
        let result = scoring.State

        printfn "%A" result

        Assert.AreEqual(6, result.Judgements.[0])
        Assert.AreEqual(1, result.Judgements.[5])
