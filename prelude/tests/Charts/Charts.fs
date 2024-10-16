namespace Prelude.Tests.Charts

open NUnit.Framework
open Prelude
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
                0, 7, "9478FFC413A3FCFB65055CAF69A6F25E9B7EF5AAA73532DF8A99D320EEAD4689"
                123, 10, "61F0F1DDE1B53E6A8382AB0879B2FF86B2CA7E0B25D29E7357BD3DCEA078EB99"
            ]

        for seed, keys, expected_hash in TEST_DATA do
            let chart = ChartFuzzBuilder.generate (keys, seed)

            Assert.AreEqual(expected_hash, Chart.hash chart)

        Assert.Pass()

    [<Test>]
    let Scale() =

        let chart = ChartFuzzBuilder.generate (4, 123)
        let twice_as_long = Chart.scale 2.0f<rate> chart
        let scaled_to_original = Chart.scale 0.5f<rate> twice_as_long

        Assert.AreNotEqual(Chart.hash twice_as_long, Chart.hash chart)
        Assert.AreEqual(Chart.hash scaled_to_original, Chart.hash chart)