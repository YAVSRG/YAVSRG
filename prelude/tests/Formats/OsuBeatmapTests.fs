namespace Prelude.Tests.Charts

open NUnit.Framework
open Prelude.Formats.Osu

module OsuBeatmapTests =

    [<Test>]
    let TimingPoint_ValidParses () =

        let inline expected_result (expected: string, input: string) =
            let result =
                try
                    OsuParser.parse_timing_point(input).ToString()
                with _ ->
                    "## PARSE ERROR"

            Assert.AreEqual(expected, result)

        let default_values =
            TimingPoint.Uninherited(
                {
                    Time = 100.1
                    MsPerBeat = 500
                    Meter = 4
                    SampleSet = SampleSet.Default
                    SampleIndex = 0
                    Volume = 100
                    Effects = TimingEffect.None
                }
            )

        Assert.AreEqual(default_values, OsuParser.parse_timing_point("100.1,"))


        expected_result("## PARSE ERROR", "")
        expected_result("## PARSE ERROR", "100.1")

        // Time
        expected_result("## PARSE ERROR", ",250.1")
        expected_result("## PARSE ERROR", "AAA,250.1")
        expected_result("## PARSE ERROR", "Nan,250.1")
        expected_result("## PARSE ERROR", "Infinity,250.1")
        expected_result("## PARSE ERROR", "-Infinity,250.1")
        expected_result("## PARSE ERROR", "2147483648,250.1")
        expected_result("## PARSE ERROR", "-2147483648,250.1")
        expected_result("-2147483647,250.1,4,0,0,100,1,0", "-2147483647,250.1")
        expected_result("100.1,250.1,4,0,0,100,1,0", " 100.1,250.1")

        // MsPerBeat
        expected_result("100.1,500,4,0,0,100,1,0", "100.1,")
        expected_result("100.1,500,4,0,0,100,1,0", "100.1,AAA")
        expected_result("100.1,500,4,0,0,100,1,0", "100.1,Nan")
        expected_result("100.1,500,4,0,0,100,1,0", "100.1,Infinity")
        expected_result("100.1,500,4,0,0,100,1,0", "100.1,-Infinity")
        expected_result("100.1,2147483647,4,0,0,100,1,0", "100.1,2147483647")
        expected_result("100.1,500,4,0,0,100,1,0", "100.1,2147483648")
        expected_result("100.1,0,4,0,0,100,1,0", "100.1,-1")
        expected_result("100.1,0,4,0,0,100,1,0", "100.1,0")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1, 250.1")

        // Meter
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,0")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,-1")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,AAA")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,Nan")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,Infinity")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,-Infinity")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,2147483648")
        expected_result("100.1,250.1,2147483647,0,0,100,1,0", "100.1,250.1,2147483647")
        expected_result("100.1,250.1,3,0,0,100,1,0", "100.1,250.1,3")
        expected_result("100.1,250.1,3,0,0,100,1,0", "100.1,250.1, 3")

        // SampleSet
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,2147483648")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,-2147483649")
        expected_result("100.1,250.1,4,2147483647,0,100,1,0", "100.1,250.1,4,2147483647")
        expected_result("100.1,250.1,4,-2147483648,0,100,1,0", "100.1,250.1,4,-2147483648")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,AAA")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,Default")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,None")
        expected_result("100.1,250.1,4,1,0,100,1,0", "100.1,250.1,4,Normal")
        expected_result("100.1,250.1,4,2,0,100,1,0", "100.1,250.1,4,Soft")
        expected_result("100.1,250.1,4,3,0,100,1,0", "100.1,250.1,4,Drum")
        expected_result("100.1,250.1,4,1,0,100,1,0", "100.1,250.1,4, 1")

        // SampleIndex
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,0,")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,0,AAA")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,0,-2147483648")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,0,2147483648")
        expected_result("100.1,250.1,4,0,-2147483647,100,1,0", "100.1,250.1,4,0,-2147483647")
        expected_result("100.1,250.1,4,0,2147483647,100,1,0", "100.1,250.1,4,0,2147483647")
        expected_result("100.1,250.1,4,0,2,100,1,0", "100.1,250.1,4,0, 2")

        // Volume
        expected_result("100.1,250.1,4,0,0,0,1,0", "100.1,250.1,4,0,0,-1")
        expected_result("100.1,250.1,4,0,0,0,1,0", "100.1,250.1,4,0,0,0")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,0,0,")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,0,0,AAA")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,0,0,Infinity")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,0,0,-Infinity")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,0,0,Nan")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,0,0,101")
        expected_result("100.1,250.1,4,0,0,50,1,0", "100.1,250.1,4,0,0, 50")

        // Uninherited
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,0,0,100,1")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,0,0,100,100")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,0,0,100,1A")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,0,0,100,1.1")
        // Inherited
        expected_result("100.1,-100,4,0,0,100,0,0", "100.1,250.1,4,0,0,100, 1") // todo: in lazer this is Uninherited: bug report?
        expected_result("100.1,-100,4,0,0,100,0,0", "100.1,250.1,4,0,0,100,\"1\"")
        expected_result("100.1,-100,4,0,0,100,0,0", "100.1,250.1,4,0,0,100,A")
        expected_result("100.1,-100,4,0,0,100,0,0", "100.1,250.1,4,0,0,100,")
        expected_result("100.1,-100,4,0,0,100,0,0", "100.1,250.1,4,0,0,100,0")
        expected_result("100.1,-100,4,0,0,100,0,0", "100.1,250.1,4,0,0,100,-1")
