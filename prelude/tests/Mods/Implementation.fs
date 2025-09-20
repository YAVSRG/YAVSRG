namespace Prelude.Tests.Mods

open NUnit.Framework
open Percyqaz.Common
open Prelude
open Prelude.Charts
open Prelude.Mods
open Prelude.Tests.Charts

module Implementation =

    let SAMPLE_CHART = ChartFuzzBuilder.generate (4, 0)

    [<Test>]
    let Mirror() =

        let mirrored_chart = ModState.apply (ModState.cycle "mirror" Map.empty) SAMPLE_CHART
        let mirrored_twice_chart = ModState.apply (ModState.cycle "mirror" Map.empty) { SAMPLE_CHART with Notes = mirrored_chart.Notes }

        Assert.AreNotEqual(Chart.hash SAMPLE_CHART, Chart.hash { SAMPLE_CHART with Notes = mirrored_chart.Notes })
        Assert.AreEqual(Chart.hash SAMPLE_CHART, Chart.hash { SAMPLE_CHART with Notes = mirrored_twice_chart.Notes })

    [<Test>]
    let ColumnSwap_Parse() =

        match ColumnSwap.parse "65436-" with
        | Ok x -> printfn "%A" x
        | Error reason -> Assert.Fail(reason)

        match ColumnSwap.parse "---" with
        | Ok x -> printfn "%A" x
        | Error reason -> Assert.Fail(reason)

        match ColumnSwap.parse "0" with
        | Error reason -> printfn "%s" reason
        | Ok x -> Assert.Fail("Unexpected", x)

        match ColumnSwap.parse "123A5" with
        | Error reason -> printfn "%s" reason
        | Ok x -> Assert.Fail("Unexpected", x)

    [<Test>]
    let ColumnSwap_Apply() =

        let swap_1 = ColumnSwap.parse "1234321" |> expect
        let swap_2 = ColumnSwap.parse "7654" |> expect

        let swapped_chart, _ = ColumnSwap.apply swap_1 (ModdedChartInternal.OfChart SAMPLE_CHART)
        let swapped_back_chart, _ = ColumnSwap.apply swap_2 swapped_chart

        Assert.AreEqual(7, swapped_chart.Keys)
        Assert.AreNotEqual(Chart.hash SAMPLE_CHART, Chart.hash { SAMPLE_CHART with Notes = swapped_chart.Notes })
        Assert.AreEqual(Chart.hash SAMPLE_CHART, Chart.hash { SAMPLE_CHART with Notes = swapped_back_chart.Notes })

    [<Test>]
    let ColumnSwap_NoNotes() =

        let swap = ColumnSwap.parse "9999" |> expect
        let _, mod_applied = ColumnSwap.apply swap (ModdedChartInternal.OfChart SAMPLE_CHART)
        Assert.False(mod_applied)

    [<Test>]
    let ColumnSwap_Pack_RoundTrip() =

        let example = ColumnSwap.parse "54325-" |> expect
        let packed = ColumnSwap.pack example
        printfn "%B" packed

        let unpacked = ColumnSwap.unpack packed

        Assert.AreEqual(example, unpacked)

        let example2 = ColumnSwap.parse "9090990909" |> expect
        let packed2 = ColumnSwap.pack example2
        printfn "%B" packed2

        let unpacked2 = ColumnSwap.unpack packed2

        Assert.AreEqual(example2, unpacked2)

    [<Test>]
    let Inverse_CreatesValidChart() =

        let inverted, _ = Inverse.apply (0.5f<beat>) (ModdedChartInternal.OfChart SAMPLE_CHART)

        match Chart.check { SAMPLE_CHART with Notes = inverted.Notes } with
        | Ok _ -> Assert.Pass()
        | Error reason ->
            Chart.pretty_print inverted.Notes
            Assert.Fail(reason)

    [<Test>]
    let Inverse_LengthFix() =
        let notes : TimeArray<NoteRow> = [|
            { Time = 0.0f<ms>; Data = [| NoteType.NORMAL; NoteType.NOTHING; NoteType.NORMAL; NoteType.NOTHING |] }
            { Time = 250.0f<ms>; Data = [| NoteType.NOTHING; NoteType.NORMAL; NoteType.NOTHING; NoteType.NORMAL |] }
            { Time = 500.0f<ms>; Data = [| NoteType.NORMAL; NoteType.NOTHING; NoteType.NORMAL; NoteType.NOTHING |] }
            { Time = 749.0f<ms>; Data = [| NoteType.NOTHING; NoteType.NORMAL; NoteType.NOTHING; NoteType.NORMAL |] }
            { Time = 1000.0f<ms>; Data = [| NoteType.NORMAL; NoteType.NOTHING; NoteType.NORMAL; NoteType.NOTHING |] }
            { Time = 1250.0f<ms>; Data = [| NoteType.NOTHING; NoteType.NORMAL; NoteType.NOTHING; NoteType.NORMAL |] }
        |]

        let chart : Chart =
            {
                Keys = 4
                Notes = notes
                BPM = [| { Time = 0.0f<ms>; Data = { Meter = 4<beat>; MsPerBeat = 250.0f<ms / beat> } } |]
                SV = [||]
            }

        let inverted, _ = Inverse.apply 1.0f<beat> (ModdedChartInternal.OfChart chart)

        Chart.pretty_print inverted.Notes

        Assert.AreEqual(8, inverted.Notes.Length)