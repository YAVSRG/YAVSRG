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

        Assert.AreEqual([| 0.2f; 0.2f |], output)

    [<Test>]
    let NotesDuringBody() =

        let notes =
            [|
                { Time = 0.0f<ms>; Data = [| NoteType.HOLDHEAD; NoteType.NOTHING |]}
                { Time = 200.0f<ms>; Data = [| NoteType.HOLDBODY; NoteType.NORMAL |]}
                { Time = 300.0f<ms>; Data = [| NoteType.HOLDBODY; NoteType.NORMAL |]}
                { Time = 1000.0f<ms>; Data = [| NoteType.HOLDTAIL; NoteType.NOTHING |]}
            |]

        let output = HoldCoverage.calculate_coverage (2, notes, 1.0f<rate>)

        Assert.AreEqual([| 0.25f; 0.45f; 0.5f; 0.25f |], output)