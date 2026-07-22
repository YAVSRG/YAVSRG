namespace Prelude.Tests.Calculator

open NUnit.Framework
open Prelude
open Prelude.Charts
open Prelude.Calculator
open Prelude.Tests.Helpers

module HoldCoverageTests =

    [<Test>]
    let NotesHaveNoHolds() =

        let note_data =
            NotesBuilder(2)
                .Note(0.0f<ms>, 0)
                .Note(100.0f<ms>, 1)
                .Build()

        let output = HoldCoverage.calculate_coverage(note_data, 1.0f<rate>)

        Assert.AreEqual([| 0.0f; 0.0f |], output)

    [<Test>]
    let NotesHaveOneHold() =

        let note_data =
            NotesBuilder(2)
                .HoldUntil(0.0f<ms>, 200.0f<ms>, 0)
                .Note(200.0f<ms>, 1)
                .Build()

        let output = HoldCoverage.calculate_coverage (note_data, 1.0f<rate>)

        let expected = (200.0f<ms> - HoldCoverage.SHORTEN_AMOUNT * 1.0f<rate>) / 1000.0f<ms>

        Assert.AreEqual([| expected;expected |], output)

    [<Test>]
    let NotesDuringBody() =
        
        // todo: extend NotesBuilder to allow simultaneous hold notes, so these test scenarios can use it

        let notes =
            [|
                { Time = 0.0f<ms>; Data = [| NoteType.HOLDHEAD; NoteType.NOTHING |]}
                { Time = 200.0f<ms>; Data = [| NoteType.HOLDBODY; NoteType.NORMAL |]}
                { Time = 300.0f<ms>; Data = [| NoteType.HOLDBODY; NoteType.NORMAL |]}
                { Time = 1000.0f<ms>; Data = [| NoteType.HOLDBODY; NoteType.NORMAL |]}
                { Time = 1000.0f<ms> + HoldCoverage.SHORTEN_AMOUNT * 1.0f<rate>; Data = [| NoteType.HOLDTAIL; NoteType.NOTHING |]}
            |]

        let output = HoldCoverage.calculate_coverage ({ Keys = 2; Notes = notes }, 1.0f<rate>)

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

        let output = HoldCoverage.calculate_coverage ({ Keys = 2; Notes = notes }, 1.0f<rate>)

        Assert.AreEqual([| 0.25f; 0.72f; 0.64f; 0.17f |], output)