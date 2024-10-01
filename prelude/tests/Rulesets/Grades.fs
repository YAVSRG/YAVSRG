namespace Prelude.Tests.Rulesets

open System
open NUnit.Framework
open Prelude
open Prelude.Gameplay.Rulesets
open Prelude.Gameplay.Scoring

module Grades =

    let GRADES =
        [|
            {
                Name = "A-"
                Accuracy = 0.93995
                Color = Color.FromArgb(148, 210, 180)
            }
            {
                Name = "A"
                Accuracy = 0.94995
                Color = Color.FromArgb(134, 227, 183)
            }
            {
                Name = "A+"
                Accuracy = 0.95995
                Color = Color.FromArgb(127, 231, 139)
            }
            {
                Name = "S-"
                Accuracy = 0.96995
                Color = Color.FromArgb(237, 205, 140)
            }
            {
                Name = "S"
                Accuracy = 0.97995
                Color = Color.FromArgb(246, 234, 128)
            }
            {
                Name = "S+"
                Accuracy = 0.98995
                Color = Color.FromArgb(235, 200, 220)
            }
        |]

    [<Test>]
    let GradeTestData_Valid() =
        for a, b in Seq.pairwise GRADES do
            if not (b.Accuracy > a.Accuracy) then Assert.Fail()
        Assert.Pass()

    [<Test>]
    let Grade_ExpectedResult_NoGrade() =
        
        let accuracy = 0.5

        let result = Grade.calculate_with_target GRADES accuracy
        printfn "%A" result

        Assert.AreEqual(-1, result.Grade)
        Assert.AreEqual(0.43995, Math.Round(result.AccuracyIncreaseForNextGrade.Value, 5))

    [<Test>]
    let Grade_ExpectedResult_AMinus() =
        
        let accuracy = 0.93995

        let result = Grade.calculate_with_target GRADES accuracy
        printfn "%A" result

        Assert.AreEqual(0, result.Grade)
        Assert.AreEqual(0.01, Math.Round(result.AccuracyIncreaseForNextGrade.Value, 5))

    [<Test>]
    let Grade_ExpectedResult_AMinus_2() =
        
        let accuracy = 0.94

        let result = Grade.calculate_with_target GRADES accuracy
        printfn "%A" result

        Assert.AreEqual(0, result.Grade)
        Assert.AreEqual(0.00995, Math.Round(result.AccuracyIncreaseForNextGrade.Value, 5))

    [<Test>]
    let Grade_ExpectedResult_A() =
        
        let accuracy = 0.955

        let result = Grade.calculate_with_target GRADES accuracy
        printfn "%A" result

        Assert.AreEqual("A", GRADES.[result.Grade].Name)
        Assert.AreEqual(0.00495, Math.Round(result.AccuracyIncreaseForNextGrade.Value, 5))

    [<Test>]
    let Grade_ExpectedResult_SPlus() =
        
        let accuracy = 0.98995

        let result = Grade.calculate_with_target GRADES accuracy
        printfn "%A" result

        Assert.AreEqual("S+", GRADES.[result.Grade].Name)
        Assert.AreEqual(None, result.AccuracyIncreaseForNextGrade)

    [<Test>]
    let Grade_ExpectedResult_SPlus_2() =
        
        let accuracy = 1.0

        let result = Grade.calculate_with_target GRADES accuracy
        printfn "%A" result

        Assert.AreEqual("S+", GRADES.[result.Grade].Name)
        Assert.AreEqual(None, result.AccuracyIncreaseForNextGrade)

    [<Test>]
    let NoGrades_NoError() =
        
        let accuracy = 0.0

        let result = Grade.calculate_with_target [||] accuracy
        printfn "%A" result

        Assert.AreEqual(-1, result.Grade)
        Assert.AreEqual(None, result.AccuracyIncreaseForNextGrade)

    [<Test>]
    let NoGrades_NoError_2() =
        
        let accuracy = 1.0

        let result = Grade.calculate_with_target [||] accuracy
        printfn "%A" result

        Assert.AreEqual(-1, result.Grade)
        Assert.AreEqual(None, result.AccuracyIncreaseForNextGrade)
