namespace Prelude.Tests.Rulesets

open NUnit.Framework
open Prelude
open Prelude.Gameplay.RulesetsV2
open Prelude.Gameplay.ScoringV2

module Scoring =

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
                .KeyDownFor(1045.0f<ms>, 30.0f<ms>)
                .Build()

        let result = ScoreProcessor.run RULESET 4 replay notes 1.0f<rate>

        printfn "Accuracy: %.2f%%" (result.Accuracy * 100.0)
        printfn "Judgements: %A" result.JudgementCounts

        Assert.AreEqual(1, result.JudgementCounts.[0])
        Assert.AreEqual(1, result.JudgementCounts.[1])
        Assert.AreEqual(0.95, result.Accuracy)
        Assert.AreEqual(2, result.CurrentCombo)
        Assert.AreEqual(result.CurrentCombo, result.MaxPossibleCombo)
        Assert.AreEqual(result.BestCombo, result.MaxPossibleCombo)
        Assert.AreEqual(0, result.ComboBreaks)
    
    [<Test>]
    let TimeStepped_Example () =
        let notes = 
            ChartBuilder(4)
                .Note(0.0f<ms>)
                .Hold(1000.0f<ms>, 2000.0f<ms>)
                .Hold(3000.0f<ms>, 4000.0f<ms>)
                .Build()

        let replay = 
            ReplayBuilder()
                .KeyDownFor(-45.0f<ms>, 30.0f<ms>)
                .KeyDownFor(1000.0f<ms>, 1000.0f<ms>)
                .KeyDownFor(3000.0f<ms>, 30.0f<ms>)
                .KeyDownFor(3100.0f<ms>, 900.0f<ms>)
                .Build()

        let stepper = ScoreProcessor.create RULESET 4 replay notes 1.0f<rate>

        stepper.OnEvent.Add (printfn "%A")

        let step time =
            printfn "STEPPING TO %.1fms:" time

            stepper.Update time

            printfn "Accuracy: %.2f%%" (stepper.Accuracy * 100.0)
            printfn "Judgements: %A" stepper.JudgementCounts

        Assert.True(stepper.JudgementCounts |> Seq.forall ((=) 0))
        Assert.AreEqual(1.0, stepper.Accuracy)
        Assert.AreEqual(0, stepper.CurrentCombo)

        step -100.0f<ms>

        Assert.True(stepper.JudgementCounts |> Seq.forall ((=) 0))
        Assert.AreEqual(1.0, stepper.Accuracy)
        Assert.AreEqual(0, stepper.CurrentCombo)

        step -45.0f<ms>

        Assert.AreEqual(1, stepper.JudgementCounts.[1])
        Assert.AreEqual(0.9, stepper.Accuracy)
        Assert.AreEqual(1, stepper.CurrentCombo)

        step 1000.0f<ms>

        Assert.AreEqual(1, stepper.JudgementCounts.[1])
        Assert.AreEqual(0.9, stepper.Accuracy)
        Assert.AreEqual(2, stepper.CurrentCombo)

        step 2000.0f<ms>

        Assert.AreEqual(1, stepper.JudgementCounts.[0])
        Assert.AreEqual(1, stepper.JudgementCounts.[1])
        Assert.AreEqual(0.95, stepper.Accuracy)
        Assert.AreEqual(3, stepper.CurrentCombo)

        step 3000.0f<ms>
        
        Assert.AreEqual(1, stepper.JudgementCounts.[0])
        Assert.AreEqual(1, stepper.JudgementCounts.[1])
        Assert.AreEqual(0.95, stepper.Accuracy)
        Assert.AreEqual(4, stepper.CurrentCombo)

        step 3050.0f<ms>
        
        Assert.AreEqual(1, stepper.JudgementCounts.[0])
        Assert.AreEqual(1, stepper.JudgementCounts.[1])
        Assert.AreEqual(0.95, stepper.Accuracy)
        Assert.AreEqual(0, stepper.CurrentCombo)
        Assert.AreEqual(4, stepper.BestCombo)
        Assert.AreEqual(4, stepper.MaxPossibleCombo)

        step 3100.0f<ms>
        
        Assert.AreEqual(1, stepper.JudgementCounts.[0])
        Assert.AreEqual(1, stepper.JudgementCounts.[1])
        Assert.AreEqual(0.95, stepper.Accuracy)
        Assert.AreEqual(0, stepper.CurrentCombo)
        Assert.AreEqual(4, stepper.BestCombo)
        Assert.AreEqual(4, stepper.MaxPossibleCombo)

        step 4000.0f<ms>
        
        Assert.AreEqual(1, stepper.JudgementCounts.[0])
        Assert.AreEqual(1, stepper.JudgementCounts.[1])
        Assert.AreEqual(1, stepper.JudgementCounts.[3])
        Assert.AreEqual(1.4 / 3.0, stepper.Accuracy)
        Assert.AreEqual(0, stepper.CurrentCombo)
        Assert.AreEqual(4, stepper.BestCombo)
        Assert.AreEqual(5, stepper.MaxPossibleCombo)

    // todo: test all ln combinations for Interlude ruleset
    // todo: test all ln combinations for osu! ruleset
    // todo: test all ln combinations for osu! score v2 ruleset
    // todo: test simple ln combinations for Etterna ruleset