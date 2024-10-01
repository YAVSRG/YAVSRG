namespace Prelude.Tests.Rulesets

open NUnit.Framework
open Prelude
open Prelude.Gameplay.Rulesets
open Prelude.Gameplay.Scoring

module Lamps =

    let LAMPS =
        [|
            {
                Name = "SDCB"
                Requirement = LampRequirement.ComboBreaksAtMost 9
                Color = Color.FromArgb(255, 160, 160)
            }
            {
                Name = "1CB"
                Requirement = LampRequirement.ComboBreaksAtMost 1
                Color = Color.FromArgb(160, 160, 160)
            }
            {
                Name = "FC"
                Requirement = LampRequirement.ComboBreaksAtMost 0
                Color = Color.FromArgb(80, 255, 80)
            }
            {
                Name = "SDG"
                Requirement = LampRequirement.JudgementAtMost (2, 9)
                Color = Color.FromArgb(160, 255, 160)
            }
            {
                Name = "1G"
                Requirement = LampRequirement.JudgementAtMost (2, 1)
                Color = Color.FromArgb(200, 160, 255)
            }
            {
                Name = "PFC"
                Requirement = LampRequirement.JudgementAtMost (2, 0)
                Color = Color.FromArgb(255, 255, 80)
            }
            {
                Name = "MFC"
                Requirement = LampRequirement.JudgementAtMost (1, 0)
                Color = Color.FromArgb(160, 255, 255)
            }
        |]

    [<Test>]
    let LampTestData_Valid() =
        for a, b in Seq.pairwise LAMPS do
            if not (b.Requirement.IsStricterThan a.Requirement) then Assert.Fail()
        Assert.Pass()

    [<Test>]
    let Lamp_ExpectedResult_NoLamp() =
        
        let judgements = [|100; 50; 20; 5; 0; 0|]
        let combo_breaks = 10

        let result = Lamp.calculate_with_target LAMPS judgements combo_breaks
        printfn "%A" result

        Assert.AreEqual(-1, result.Lamp)
        Assert.AreEqual(LampImprovement.FewerComboBreaks 1, result.NextLampGoal)
    
    [<Test>]
    let Lamp_ExpectedResult_SDCB_1() =
        
        let judgements = [|100; 50; 20; 5; 0; 0|]
        let combo_breaks = 9

        let result = Lamp.calculate_with_target LAMPS judgements combo_breaks
        printfn "%A" result

        Assert.AreEqual("SDCB", LAMPS.[result.Lamp].Name)
        Assert.AreEqual(LampImprovement.FewerComboBreaks 8, result.NextLampGoal)
    
    [<Test>]
    let Lamp_ExpectedResult_SDCB_2() =
        
        let judgements = [|100; 50; 20; 5; 0; 0|]
        let combo_breaks = 2

        let result = Lamp.calculate_with_target LAMPS judgements combo_breaks
        printfn "%A" result

        Assert.AreEqual("SDCB", LAMPS.[result.Lamp].Name)
        Assert.AreEqual(LampImprovement.FewerComboBreaks 1, result.NextLampGoal)
    
    [<Test>]
    let Lamp_ExpectedResult_1CB() =
        
        let judgements = [|100; 50; 20; 5; 0; 0|]
        let combo_breaks = 1

        let result = Lamp.calculate_with_target LAMPS judgements combo_breaks
        printfn "%A" result

        Assert.AreEqual("1CB", LAMPS.[result.Lamp].Name)
        Assert.AreEqual(LampImprovement.FewerComboBreaks 1, result.NextLampGoal)

    [<Test>]
    let Lamp_ExpectedResult_FC() =
        
        let judgements = [|100; 50; 20; 5; 0; 0|]
        let combo_breaks = 0

        let result = Lamp.calculate_with_target LAMPS judgements combo_breaks
        printfn "%A" result

        Assert.AreEqual("FC", LAMPS.[result.Lamp].Name)
        Assert.AreEqual(LampImprovement.FewerOfJudgement (3, 5), result.NextLampGoal)

    [<Test>]
    let Lamp_ExpectedResult_FC_2() =
        
        let judgements = [|100; 50; 20; 0; 0; 0|]
        let combo_breaks = 0

        let result = Lamp.calculate_with_target LAMPS judgements combo_breaks
        printfn "%A" result

        Assert.AreEqual("FC", LAMPS.[result.Lamp].Name)
        Assert.AreEqual(LampImprovement.FewerOfJudgement (2, 11), result.NextLampGoal)

    [<Test>]
    let Lamp_ExpectedResult_FC_3() =
        
        let judgements = [|100; 50; 10; 0; 0; 0|]
        let combo_breaks = 0

        let result = Lamp.calculate_with_target LAMPS judgements combo_breaks
        printfn "%A" result

        Assert.AreEqual("FC", LAMPS.[result.Lamp].Name)
        Assert.AreEqual(LampImprovement.FewerOfJudgement (2, 1), result.NextLampGoal)

    [<Test>]
    let Lamp_ExpectedResult_SDG() =
        
        let judgements = [|100; 50; 9; 0; 0; 0|]
        let combo_breaks = 0

        let result = Lamp.calculate_with_target LAMPS judgements combo_breaks
        printfn "%A" result

        Assert.AreEqual("SDG", LAMPS.[result.Lamp].Name)
        Assert.AreEqual(LampImprovement.FewerOfJudgement (2, 8), result.NextLampGoal)

    [<Test>]
    let Lamp_ExpectedResult_SDG_2() =
        
        let judgements = [|100; 50; 2; 0; 0; 0|]
        let combo_breaks = 0

        let result = Lamp.calculate_with_target LAMPS judgements combo_breaks
        printfn "%A" result

        Assert.AreEqual("SDG", LAMPS.[result.Lamp].Name)
        Assert.AreEqual(LampImprovement.FewerOfJudgement (2, 1), result.NextLampGoal)

    [<Test>]
    let Lamp_ExpectedResult_1G() =
        
        let judgements = [|100; 50; 1; 0; 0; 0|]
        let combo_breaks = 0

        let result = Lamp.calculate_with_target LAMPS judgements combo_breaks
        printfn "%A" result

        Assert.AreEqual("1G", LAMPS.[result.Lamp].Name)
        Assert.AreEqual(LampImprovement.FewerOfJudgement (2, 1), result.NextLampGoal)

    [<Test>]
    let Lamp_ExpectedResult_PFC() =
        
        let judgements = [|100; 50; 0; 0; 0; 0|]
        let combo_breaks = 0

        let result = Lamp.calculate_with_target LAMPS judgements combo_breaks
        printfn "%A" result

        Assert.AreEqual("PFC", LAMPS.[result.Lamp].Name)
        Assert.AreEqual(LampImprovement.FewerOfJudgement (1, 50), result.NextLampGoal)

    [<Test>]
    let Lamp_ExpectedResult_MFC() =
        
        let judgements = [|100; 0; 0; 0; 0; 0|]
        let combo_breaks = 0

        let result = Lamp.calculate_with_target LAMPS judgements combo_breaks
        printfn "%A" result

        Assert.AreEqual("MFC", LAMPS.[result.Lamp].Name)
        Assert.AreEqual(LampImprovement.AchievedMax, result.NextLampGoal)

    [<Test>]
    let Lamp_ExpectedResult_MFC_2() =
        
        let judgements = [|0; 0; 0; 0; 0; 0|]
        let combo_breaks = 0

        let result = Lamp.calculate_with_target LAMPS judgements combo_breaks
        printfn "%A" result

        Assert.AreEqual("MFC", LAMPS.[result.Lamp].Name)
        Assert.AreEqual(LampImprovement.AchievedMax, result.NextLampGoal)

    [<Test>]
    let NoLamps_NoError() =
        
        let judgements = [|0; 0; 0; 0; 0; 0|]
        let combo_breaks = 0

        let result = Lamp.calculate_with_target [||] judgements combo_breaks
        printfn "%A" result

        Assert.AreEqual(-1, result.Lamp)
        Assert.AreEqual(LampImprovement.AchievedMax, result.NextLampGoal)

    [<Test>]
    let NoLamps_NoError_2() =
        
        let judgements = [|1; 1; 1; 1; 1; 1|]
        let combo_breaks = 1

        let result = Lamp.calculate_with_target [||] judgements combo_breaks
        printfn "%A" result

        Assert.AreEqual(-1, result.Lamp)
        Assert.AreEqual(LampImprovement.AchievedMax, result.NextLampGoal)
