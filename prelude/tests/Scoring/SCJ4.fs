namespace Prelude.Tests.Scoring

open NUnit.Framework
open Prelude
open Prelude.Gameplay
open Prelude.Gameplay.PremadeRulesets
open Helpers

module ``SC J4`` =

    let RULESET = SC.create 4

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

        Assert.AreEqual(0, result.BestCombo)
        Assert.AreEqual(0, result.CurrentCombo)
        Assert.AreEqual(1, result.ComboBreaks)
        Assert.AreEqual(1, result.Judgements.[3])

    [<Test>]
    let HoldNote_Underhold () =
        let notes = ChartBuilder(4).Hold(0.0f<ms>, 1000.0f<ms>).Build()
        let replay = ReplayBuilder().KeyDown(0.0f<ms>, 0).KeyUp(500.0f<ms>, 0).Build()
        let scoring = ScoreProcessor.run RULESET 4 replay notes 1.0f
        let result = scoring.State

        printfn "%A" result

        Assert.AreEqual(0, result.BestCombo)
        Assert.AreEqual(0, result.CurrentCombo)
        Assert.AreEqual(1, result.ComboBreaks)
        Assert.AreEqual(1, result.Judgements.[3])

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

        Assert.AreEqual(0, result.BestCombo)
        Assert.AreEqual(0, result.CurrentCombo)
        Assert.AreEqual(1, result.ComboBreaks)
        Assert.AreEqual(1, result.Judgements.[3])

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
        Assert.AreEqual(1, result.Judgements.[5])

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
