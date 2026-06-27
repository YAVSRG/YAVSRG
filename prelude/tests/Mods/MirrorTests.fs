namespace Prelude.Tests.Mods

open NUnit.Framework
open Prelude.Charts
open Prelude.Mods
open Prelude.Tests.Helpers

module MirrorTests =

    let SAMPLE_CHART = ChartFuzzer.Generate(4, 0)

    [<Test>]
    let Mirror() =

        let mirrored_chart = ModState.apply (ModState.cycle_fd "mirror" Map.empty) SAMPLE_CHART
        let mirrored_twice_chart = ModState.apply (ModState.cycle_fd "mirror" Map.empty) { SAMPLE_CHART with Notes = mirrored_chart.Notes }

        Assert.AreNotEqual(Chart.hash SAMPLE_CHART, Chart.hash { SAMPLE_CHART with Notes = mirrored_chart.Notes })
        Assert.AreEqual(Chart.hash SAMPLE_CHART, Chart.hash { SAMPLE_CHART with Notes = mirrored_twice_chart.Notes })