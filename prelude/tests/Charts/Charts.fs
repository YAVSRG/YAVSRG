namespace Prelude.Tests.Charts

open NUnit.Framework
open Prelude.Charts

module Charts =

    [<Test>]
    let Fuzz_CreatesValidCharts() =

        for seed, keys in [ 123, 4; 0, 7; 123, 10 ] do
            let chart = ChartFuzzBuilder.generate (keys, seed)
            match Chart.check chart with
            | Ok _ -> ()
            | Error reason -> Assert.Fail(reason)

        Assert.Pass()

    [<Test>]
    let Fuzz_PrettyPrint() =

        let chart = ChartFuzzBuilder.generate (5, 0)

        Chart.pretty_print chart

        Assert.Pass()

    [<Test>]
    let Fuzz_Deterministic_Hashing_Deterministic() =
    
        let TEST_DATA =
            [
                123, 4, "7F7ED43C7820701F541548FCA9A6692B0B6109A9F0D78FDFBE84D2BEA3E0A4F0"
                0, 7, "08437AC86AAE95A232BB157F1146D079444103088B581053EDD2EA7945A53273"
                123, 10, "46165D72153B00DEC07F6C77005A7AEA99066EE883D2D9409A4A812ABCBF2403"
            ]

        for seed, keys, expected_hash in TEST_DATA do
            let chart = ChartFuzzBuilder.generate (keys, seed)

            Assert.AreEqual(expected_hash, Chart.hash chart)

        Assert.Pass()

    [<Test>]
    let Scale() =

        let chart = ChartFuzzBuilder.generate (4, 123)
        let twice_as_long = Chart.scale 2.0f chart
        let scaled_to_original = Chart.scale 0.5f twice_as_long

        Assert.AreNotEqual(Chart.hash twice_as_long, Chart.hash chart)
        Assert.AreEqual(Chart.hash scaled_to_original, Chart.hash chart)