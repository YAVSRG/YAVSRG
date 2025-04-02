namespace Prelude.Tests.Calculator

open NUnit.Framework
open Prelude
open Prelude.Charts
open Prelude.Calculator

module HoldCoverage =

    [<Test>]
    let NotesHaveNoHolds() =

        let notes =
            [|
                { Time = 0.0f<ms>; Data = [| NoteType.NORMAL; NoteType.NOTHING |]}
                { Time = 100.0f<ms>; Data = [| NoteType.NOTHING; NoteType.NORMAL |]}
            |]

        let output = HoldCoverage.calculate_coverage (2, notes, 1.0f<rate>)

        Assert.AreEqual([| 0.0f; 0.0f |], output)

    [<Test>]
    let NotesHaveOneHold() =

        let notes =
            [|
                { Time = 0.0f<ms>; Data = [| NoteType.HOLDHEAD; NoteType.NOTHING |]}
                { Time = 200.0f<ms>; Data = [| NoteType.HOLDTAIL; NoteType.NORMAL |]}
            |]

        let output = HoldCoverage.calculate_coverage (2, notes, 1.0f<rate>)

        let expected = (200.0f<ms> - HoldCoverage.SHORTEN_AMOUNT * 1.0f<rate>) / 1000.0f<ms>

        Assert.AreEqual([| expected;expected |], output)

    [<Test>]
    let NotesDuringBody() =

        let notes =
            [|
                { Time = 0.0f<ms>; Data = [| NoteType.HOLDHEAD; NoteType.NOTHING |]}
                { Time = 200.0f<ms>; Data = [| NoteType.HOLDBODY; NoteType.NORMAL |]}
                { Time = 300.0f<ms>; Data = [| NoteType.HOLDBODY; NoteType.NORMAL |]}
                { Time = 1000.0f<ms>; Data = [| NoteType.HOLDBODY; NoteType.NORMAL |]}
                { Time = 1000.0f<ms> + HoldCoverage.SHORTEN_AMOUNT * 1.0f<rate>; Data = [| NoteType.HOLDTAIL; NoteType.NOTHING |]}
            |]

        let output = HoldCoverage.calculate_coverage (2, notes, 1.0f<rate>)

        Assert.AreEqual([| 0.25f; 0.45f; 0.5f; 0.25f; 0.17f |], output)

    [<Test>]
    let TwoHoldNotes() =

        let notes =
            [|
                { Time = 0.0f<ms>; Data = [| NoteType.HOLDHEAD; NoteType.NOTHING |]}
                { Time = 300.0f<ms>; Data = [| NoteType.HOLDBODY; NoteType.HOLDHEAD |]}
                { Time = 600.0f<ms>; Data = [| NoteType.HOLDTAIL; NoteType.HOLDBODY |]}
                { Time = 900.0f<ms>; Data = [| NoteType.NOTHING; NoteType.HOLDTAIL |]}
            |]

        let output = HoldCoverage.calculate_coverage (2, notes, 1.0f<rate>)

        Assert.AreEqual([| 0.25f; 0.72f; 0.64f; 0.17f |], output)