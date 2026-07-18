namespace Prelude.Tests.Mods

open NUnit.Framework
open Prelude
open Prelude.Charts
open Prelude.Mods
open Prelude.Tests.Helpers

module InverseTests =

    let SAMPLE_CHART = ChartFuzzer.Generate(4, 0)

    [<Test>]
    let Inverse_CreatesValidChart () =

        let inverted, _ =
            Inverse.apply 0.5f<beat> (ModdedChartInternal.OfChart(SAMPLE_CHART))

        match { SAMPLE_CHART with Notes = inverted.Notes }.CheckForErrors() with
        | Ok _ -> Assert.Pass()
        | Error reason ->
            Chart.pretty_print inverted.Notes
            Assert.Fail(reason)

    [<Test>]
    let Inverse_LengthFix () =
        let notes: TimeArray<NoteRow> =
            [|
                { Time = 0.0f<ms>; Data = [| NoteType.NORMAL; NoteType.NOTHING; NoteType.NORMAL; NoteType.NOTHING |] }
                { Time = 250.0f<ms>; Data = [| NoteType.NOTHING; NoteType.NORMAL; NoteType.NOTHING; NoteType.NORMAL |] }
                { Time = 500.0f<ms>; Data = [| NoteType.NORMAL; NoteType.NOTHING; NoteType.NORMAL; NoteType.NOTHING |] }
                { Time = 749.0f<ms>; Data = [| NoteType.NOTHING; NoteType.NORMAL; NoteType.NOTHING; NoteType.NORMAL |] }
                {
                    Time = 1000.0f<ms>
                    Data = [| NoteType.NORMAL; NoteType.NOTHING; NoteType.NORMAL; NoteType.NOTHING |]
                }
                {
                    Time = 1250.0f<ms>
                    Data = [| NoteType.NOTHING; NoteType.NORMAL; NoteType.NOTHING; NoteType.NORMAL |]
                }
            |]

        let chart: Chart =
            {
                Keys = 4
                Notes = notes
                BPM =
                    [|
                        { Time = 0.0f<ms>; Data = { Meter = 4<beat>; MsPerBeat = 250.0f<ms / beat> } }
                    |]
                SV = [||]
            }

        let inverted, _ = Inverse.apply 1.0f<beat> (ModdedChartInternal.OfChart(chart))

        Chart.pretty_print inverted.Notes

        Assert.AreEqual(8, inverted.Notes.Length)
