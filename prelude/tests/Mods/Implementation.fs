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

        match ColumnSwap.parse "54325-" with
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

        let swap_1 = ColumnSwap.parse "0123210" |> expect
        let swap_2 = ColumnSwap.parse "6543" |> expect

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
    let Inverse() =

        let inverted, _ = Inverse.apply (0.5f<beat>) (ModdedChartInternal.OfChart SAMPLE_CHART)

        match Chart.check { SAMPLE_CHART with Notes = inverted.Notes } with
        | Ok _ -> Assert.Pass()
        | Error reason ->
            Chart.pretty_print inverted.Notes
            Assert.Fail(reason)