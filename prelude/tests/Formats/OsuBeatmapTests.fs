namespace Prelude.Tests.Formats

open NUnit.Framework
open Prelude.Formats.Osu

module OsuBeatmapTests =

    // Most test cases represent what osu! would do if it parsed this input, and then wrote it back to an .osu file
    // Try pasting these test cases into your .osu files, hitting CTRL-L in the editor, then CTRL-S and seeing what the client writes back

    // ## PARSE ERROR indicates that:
    // osu! editor reports an error on load for this input
    // OR osu! will refuse to play a file containing data loaded like this (try F5 in the editor)
    // OR osu! will skip over/ignore this line on load and it is removed when writing back
    // OR I have ruled that accepting this input is not a good idea even when allowed by osu!

    [<Test>]
    let TimingPoint_ValidParses () =

        let inline timing_point_tc (expected: string, input: string) =
            let result =
                try
                    TimingPoint.FromString(input).ToString()
                with _ ->
                    "## PARSE ERROR"

            Assert.AreEqual(expected, result)

        let default_values =
            TimingPoint.Uninherited(
                {
                    Time = 100.1
                    MsPerBeat = 250.1
                    Meter = 4
                    SampleSet = SampleSet.Soft
                    SampleIndex = 0
                    Volume = 100
                    Effects = TimingEffect.None
                }
            )

        Assert.AreEqual(default_values, TimingPoint.FromString("100.1,250.1"))

        timing_point_tc("## PARSE ERROR", "")
        timing_point_tc("## PARSE ERROR", "100.1")
        timing_point_tc("## PARSE ERROR", "100.1,")

        // Time
        timing_point_tc("## PARSE ERROR", ",250.1")
        timing_point_tc("## PARSE ERROR", "AAA,250.1")
        timing_point_tc("## PARSE ERROR", "NaN,250.1")
        timing_point_tc("1E+100,250.1,4,2,0,100,1,0", "1e100,250.1")
        timing_point_tc("-1E+100,250.1,4,2,0,100,1,0", "-1e100,250.1")
        timing_point_tc("2147483648,250.1,4,2,0,100,1,0", "2147483648,250.1")
        timing_point_tc("-2147483649,250.1,4,2,0,100,1,0", "-2147483649,250.1")
        timing_point_tc("-2147483648,250.1,4,2,0,100,1,0", "-2147483648,250.1")
        timing_point_tc("100.1,250.1,4,2,0,100,1,0", " 100.1 ,250.1")
        timing_point_tc("2147483647,250,4,2,0,100,1,0", "2147483647,250,4,2,0,100,1,0")

        // MsPerBeat
        timing_point_tc("## PARSE ERROR", "100.1,")
        timing_point_tc("## PARSE ERROR", "100.1,AAA")
        timing_point_tc("## PARSE ERROR", "100.1,0")
        timing_point_tc("100.1,NaN,4,2,0,100,1,0", "100.1,NaN")
        timing_point_tc("100.1,1E+100,4,2,0,100,1,0", "100.1,1e100")
        timing_point_tc("100.1,2147483648,4,2,0,100,1,0", "100.1,2147483648")
        timing_point_tc("100.1,-1E+100,4,2,0,100,1,0", "100.1,-1e100")
        timing_point_tc("100.1,-1,4,2,0,100,1,0", "100.1,-1")
        timing_point_tc("100.1,250.1,4,2,0,100,1,0", "100.1, \t250.1 ")

        // Meter
        timing_point_tc("## PARSE ERROR", "100.1,250.1,4")
        timing_point_tc("## PARSE ERROR", "100.1,250.1,,2,0,100,1,0")
        timing_point_tc("## PARSE ERROR", "100.1,250.1,1.1,2,0,100,1,0")
        timing_point_tc("## PARSE ERROR", "100.1,250.1,AAA,2,0,100,1,0")
        timing_point_tc("## PARSE ERROR", "100.1,250.1,NaN,2,0,100,1,0")
        timing_point_tc("## PARSE ERROR", "100.1,250.1,1e100,2,0,100,1,0")
        timing_point_tc("## PARSE ERROR", "100.1,250.1,-1e100,2,0,100,1,0")
        timing_point_tc("## PARSE ERROR", "100.1,250.1,2147483648,2,0,100,1,0")
        timing_point_tc("100.1,250.1,2147483647,2,0,100,1,0", "100.1,250.1,2147483647,2,0,100,1,0")
        timing_point_tc("100.1,250.1,-2147483648,2,0,100,1,0", "100.1,250.1,-2147483648,2,0,100,1,0")
        timing_point_tc("## PARSE ERROR", "100.1,250.1,-2147483649,2,0,100,1,0")
        timing_point_tc("100.1,250.1,3,2,0,100,1,0", "100.1,250.1,3,2,0,100,1,0")
        timing_point_tc("100.1,250.1,3,2,0,100,1,0", "100.1,250.1, 3 ,2,0,100,1,0")
        timing_point_tc("100.1,250.1,4,2,0,100,1,0", "100.1,250.1,0,2,0,100,1,0")
        timing_point_tc("100.1,250.1,-1,2,0,100,1,0", "100.1,250.1,-1,2,0,100,1,0")

        // SampleSet
        timing_point_tc("## PARSE ERROR", "100.1,250.1,4,2147483648,0,100,1,0")
        timing_point_tc("100.1,250.1,4,2147483647,0,100,1,0", "100.1,250.1,4,2147483647,0,100,1,0")
        timing_point_tc("100.1,250.1,4,-1,0,100,1,0", "100.1,250.1,4,-1,0,100,1,0")
        timing_point_tc("100.1,250.1,4,-2147483648,0,100,1,0", "100.1,250.1,4,-2147483648,0,100,1,0")
        timing_point_tc("## PARSE ERROR", "100.1,250.1,4,-2147483649,0,100,1,0")
        timing_point_tc("## PARSE ERROR", "100.1,250.1,4,,0,100,1,0")
        timing_point_tc("## PARSE ERROR", "100.1,250.1,4,1.1,0,100,1,0")
        timing_point_tc("## PARSE ERROR", "100.1,250.1,4,Soft,0,100,1,0")
        timing_point_tc("100.1,250.1,4,1,0,100,1,0", "100.1,250.1,4,  1  ,0,100,1,0")
        timing_point_tc("100.1,250.1,4,2,0,100,1,0", "100.1,250.1,4,0,0,100,1,0")

        // SampleIndex
        timing_point_tc("## PARSE ERROR", "100.1,250.1,4,0,2147483648,100,1,0")
        timing_point_tc("100.1,250.1,4,2,2147483647,100,1,0", "100.1,250.1,4,2,2147483647,100,1,0")
        timing_point_tc("100.1,250.1,4,2,-1,100,1,0", "100.1,250.1,4,2,-1,100,1,0")
        timing_point_tc("100.1,250.1,4,2,-2147483648,100,1,0", "100.1,250.1,4,2,-2147483648,100,1,0")
        timing_point_tc("## PARSE ERROR", "100.1,250.1,4,2,-2147483649,100,1,0")
        timing_point_tc("## PARSE ERROR", "100.1,250.1,4,2,,100,1,0")
        timing_point_tc("## PARSE ERROR", "100.1,250.1,4,2,1.1,100,1,0")
        timing_point_tc("## PARSE ERROR", "100.1,250.1,4,2,AAA,100,1,0")
        timing_point_tc("100.1,250.1,4,2,1,100,1,0", "100.1,250.1,4,2,  1  ,100,1,0")

        // Volume
        timing_point_tc("100.1,250.1,4,2,0,1,1,0", "100.1,250.1,4,2,0,-1")
        timing_point_tc("100.1,250.1,4,2,0,1,1,0", "100.1,250.1,4,2,0,0")
        timing_point_tc("## PARSE ERROR", "100.1,250.1,4,2,0,")
        timing_point_tc("## PARSE ERROR", "100.1,250.1,4,2,0,AAA")
        timing_point_tc("## PARSE ERROR", "100.1,250.1,4,2,0,1e100")
        timing_point_tc("## PARSE ERROR", "100.1,250.1,4,2,0,NaN")
        timing_point_tc("100.1,250.1,4,2,0,100,1,0", "100.1,250.1,4,2,0,101")
        timing_point_tc("100.1,250.1,4,2,0,100,1,0", "100.1,250.1,4,2,0,2147483647")
        timing_point_tc("## PARSE ERROR", "100.1,250.1,4,2,0,2147483648")
        timing_point_tc("100.1,250.1,4,2,0,50,1,0", "100.1,250.1,4,2,0, 50")

        // Uninherited
        timing_point_tc("100.1,250.1,4,2,0,100,1,0", "100.1,250.1,4,2,0,100,1")
        timing_point_tc("100.1,250.1,4,2,0,100,1,0", "100.1,250.1,4,2,0,100,100")
        timing_point_tc("100.1,250.1,4,2,0,100,1,0", "100.1,250.1,4,2,0,100,1A")
        timing_point_tc("100.1,250.1,4,2,0,100,1,0", "100.1,250.1,4,2,0,100,1.1")
        // Inherited
        timing_point_tc("100.1,-100,4,2,0,100,0,0", "100.1,250.1,4,2,0,100,\"1\"")
        // In osu!lazer this test case parses as Uninherited -- todo: bug report?
        timing_point_tc("100.1,-100,4,2,0,100,0,0", "100.1,250.1,4,2,0,100, 1")
        timing_point_tc("100.1,-100,4,2,0,100,0,0", "100.1,250.1,4,2,0,100,AAA")
        timing_point_tc("100.1,-100,4,2,0,100,0,0", "100.1,250.1,4,2,0,100,")
        timing_point_tc("100.1,-100,4,2,0,100,0,0", "100.1,250.1,4,2,0,100,0")
        timing_point_tc("100.1,-100,4,2,0,100,0,0", "100.1,250.1,4,2,0,100,2")
        timing_point_tc("100.1,-100,4,2,0,100,0,0", "100.1,250.1,4,2,0,100,-1")
        timing_point_tc("## PARSE ERROR", "100.1,-100,EVEN_THOUGH_IGNORED,2,0,100,0")
        // Inherited: Multiplier
        timing_point_tc("100.1,-50,4,2,0,100,0,0", "100.1,-50,4,2,0,100,0")
        timing_point_tc("100.1,-1,4,2,0,100,0,0", "100.1,-0.1,4,2,0,100,0")
        timing_point_tc("100.1,-10000,4,2,0,100,0,0", "100.1,-100000,4,2,0,100,0")
        timing_point_tc("100.1,-100,4,2,0,100,0,0", "100.1,0,4,2,0,100,0")
        timing_point_tc("100.1,-100,4,2,0,100,0,0", "100.1,NaN,4,2,0,100,0")
        timing_point_tc("100.1,-100,4,2,0,100,0,0", "100.1,1e100,4,2,0,100,0")
        timing_point_tc("100.1,-1,4,2,0,100,0,0", "100.1,-1e-100,4,2,0,100,0")
        timing_point_tc("100.1,-10000,4,2,0,100,0,0", "100.1,-1e100,4,2,0,100,0")

        // Effects
        timing_point_tc("## PARSE ERROR", "100.1,250.1,4,2,0,100,1,-2147483649")
        timing_point_tc("## PARSE ERROR", "100.1,250.1,4,2,0,100,1,2147483648")
        timing_point_tc("100.1,250.1,4,2,0,100,1,-2147483648", "100.1,250.1,4,2,0,100,1,-2147483648")
        timing_point_tc("100.1,250.1,4,2,0,100,1,2147483647", "100.1,250.1,4,2,0,100,1,2147483647")
        timing_point_tc("## PARSE ERROR", "100.1,250.1,4,2,0,100,1,")
        timing_point_tc("## PARSE ERROR", "100.1,250.1,4,2,0,100,1,AAA")
        timing_point_tc("## PARSE ERROR", "100.1,250.1,4,2,0,100,1,1.1")
        timing_point_tc("100.1,250.1,4,2,0,100,1,1", "100.1,250.1,4,2,0,100,1, 1")

    [<Test>]
    let HitSample_ValidParses () =

        let inline hitsample_tc (expected: string, input: string) =
            let result =
                try
                    HitSample.FromString(input).ToString()
                with _ ->
                    "## PARSE ERROR"

            Assert.AreEqual(expected, result)

        hitsample_tc("## PARSE ERROR", "")
        hitsample_tc("## PARSE ERROR", " ")
        hitsample_tc("0:0:0:0:", " 0 :  0  :   0   :    0    :")
        hitsample_tc("0:0:0:0:", "00:00:00:00:")
        hitsample_tc("-1:5:-2147483648:100: File", "-1 : 5 : -2147483648 : 101 : File")

        hitsample_tc("## PARSE ERROR", "0:0:0:-2147483649:")
        hitsample_tc("## PARSE ERROR", "0:0:0:2147483648:")
        hitsample_tc("0:0:0:0:", "0:0:0:-2147483648:")
        hitsample_tc("0:0:0:100:", "0:0:0:2147483647:")

    let inline hitobject_tc (expected: string, input: string) =
        let result =
            try
                HitObject.FromString(input).ToString()
            with _ ->
                "## PARSE ERROR"

        Assert.AreEqual(expected, result)

    [<Test>]
    let HitObject_ValidParses () =

        hitobject_tc("## PARSE ERROR", " 256,192,0,1,0")
        hitobject_tc("256,192,0,1,0,0:0:0:0:", "256 , 192 , 0 , 1 , 0")
        hitobject_tc("256,192,0,1,0,0:0:0:0:", "\t256\t,\t192\t,\t0\t,\t1,\t0\t")
        hitobject_tc("## PARSE ERROR", "256,192,0,1,0,")
        hitobject_tc("## PARSE ERROR", "256,192,0,1,0,0:0:0:0")
        hitobject_tc("256,192,0,1,0,1:1:1:1:", "256,192,0,1,0,1:1:1:1:")

    [<Test>]
    let HitObject_ValidParses_X () =

        hitobject_tc("256,192,0,1,0,0:0:0:0:", "256.9,192,0,1,0")
        hitobject_tc("## PARSE ERROR", "1e40,192,0,1,0")
        hitobject_tc("512,192,0,1,0,0:0:0:0:", "1e38,192,0,1,0")
        hitobject_tc("512,192,0,1,0,0:0:0:0:", "2147483648,192,0,1,0")
        hitobject_tc("512,192,0,1,0,0:0:0:0:", "2147483647,192,0,1,0")
        hitobject_tc("0,192,0,1,0,0:0:0:0:", "-2147483648,192,0,1,0")
        hitobject_tc("0,192,0,1,0,0:0:0:0:", "-2147483649,192,0,1,0")
        hitobject_tc("0,192,0,1,0,0:0:0:0:", "-1e38,192,0,1,0")
        hitobject_tc("## PARSE ERROR", "-1e40,192,0,1,0")
        hitobject_tc("## PARSE ERROR", "NaN,192,0,1,0")
        hitobject_tc("## PARSE ERROR", "AAA,192,0,1,0")
        hitobject_tc("## PARSE ERROR", ",192,0,1,0")

    [<Test>]
    let HitObject_ValidParses_Y () =

        hitobject_tc("256,192,0,1,0,0:0:0:0:", "256,192.9,0,1,0")
        hitobject_tc("## PARSE ERROR", "256,1e40,0,1,0")
        hitobject_tc("256,512,0,1,0,0:0:0:0:", "256,1e38,0,1,0")
        hitobject_tc("256,512,0,1,0,0:0:0:0:", "256,2147483648,0,1,0")
        hitobject_tc("256,512,0,1,0,0:0:0:0:", "256,2147483647,0,1,0")
        hitobject_tc("256,0,0,1,0,0:0:0:0:", "256,-2147483648,0,1,0")
        hitobject_tc("256,0,0,1,0,0:0:0:0:", "256,-2147483649,0,1,0")
        hitobject_tc("256,0,0,1,0,0:0:0:0:", "256,-1e38,0,1,0")
        hitobject_tc("## PARSE ERROR", "256,-1e40,0,1,0")
        hitobject_tc("## PARSE ERROR", "256,NaN,0,1,0")
        hitobject_tc("## PARSE ERROR", "256,AAA,0,1,0")
        hitobject_tc("## PARSE ERROR", "256,,0,1,0")

    [<Test>]
    let HitObject_ValidParses_Time () =

        hitobject_tc("256,192,100,1,0,0:0:0:0:", "256,192,100.1,1,0")
        hitobject_tc("## PARSE ERROR", "256,192,1e40,1,0")
        hitobject_tc("## PARSE ERROR", "256,192,1e38,1,0")
        // osu! will strip out notes that are beyond the end of the audio file or have negative timestamps
        // Interlude will not but would reject them later as part of conversion
        // Therefore no real way to verify what is intended behaviour here
        hitobject_tc("256,192,2147483647,1,0,0:0:0:0:", "256,192,2147483648,1,0")
        hitobject_tc("256,192,2147483647,1,0,0:0:0:0:", "256,192,2147483647,1,0")
        hitobject_tc("256,192,-2147483648,1,0,0:0:0:0:", "256,192,-2147483648,1,0")
        hitobject_tc("256,192,-2147483648,1,0,0:0:0:0:", "256,192,-2147483649,1,0")
        hitobject_tc("## PARSE ERROR", "256,192,-1e38,1,0")
        hitobject_tc("## PARSE ERROR", "256,192,-1e40,1,0")
        hitobject_tc("## PARSE ERROR", "256,192,NaN,1,0")
        hitobject_tc("## PARSE ERROR", "256,192,AAA,1,0")
        hitobject_tc("## PARSE ERROR", "256,192,,1,0")

    [<Test>]
    let HitObject_ValidParses_Type () =

        hitobject_tc("## PARSE ERROR", "256,192,100,0,0")
        hitobject_tc("## PARSE ERROR", "256,192,100,1.0,0")
        hitobject_tc("## PARSE ERROR", "256,192,100,,0")
        hitobject_tc("## PARSE ERROR", "256,192,100,AAA,0")

        hitobject_tc("256,192,100,1,0,0:0:0:0:", "256,192,100,1,0")
        hitobject_tc("256,192,100,1,0,0:0:0:0:", "256,192,100,3,0")
        hitobject_tc("256,192,100,1,0,0:0:0:0:", "256,192,100,9,0")
        hitobject_tc("256,192,100,1,0,0:0:0:0:", "256,192,100,129,0")
        hitobject_tc("256,192,100,1,0,0:0:0:0:", "256,192,100,137,0")
        hitobject_tc("256,192,100,1,0,0:0:0:0:", "256,192,100,139,0")
        hitobject_tc("256,192,100,5,0,0:0:0:0:", "256,192,100,5,0")
        hitobject_tc("256,192,100,5,0,0:0:0:0:", "256,192,100,13,0")
        hitobject_tc("256,192,100,5,0,0:0:0:0:", "256,192,100,133,0")

    [<Test>]
    let HitObject_ValidParses_Spinner_EndTime () =

        hitobject_tc("256,192,100,8,0,100,0:0:0:0:", "256,192,100,8,0,100.1")
        hitobject_tc("256,192,100,8,0,100,0:0:0:0:", "256,192,100,8,0, \t100 ")
        hitobject_tc("256,192,100,8,0,-2147483648,0:0:0:0:", "256,192,100,8,0,1e40")
        hitobject_tc("256,192,100,8,0,-2147483648,0:0:0:0:", "256,192,100,8,0,1e38")
        hitobject_tc("256,192,100,8,0,2147483647,0:0:0:0:", "256,192,100,8,0,2147483648")
        hitobject_tc("256,192,100,8,0,2147483647,0:0:0:0:", "256,192,100,8,0,2147483647")
        hitobject_tc("256,192,100,8,0,-2147483648,0:0:0:0:", "256,192,100,8,0,-2147483648")
        hitobject_tc("256,192,100,8,0,-2147483648,0:0:0:0:", "256,192,100,8,0,-2147483649")
        hitobject_tc("256,192,100,8,0,-2147483648,0:0:0:0:", "256,192,100,8,0,-1e38")
        hitobject_tc("256,192,100,8,0,-2147483648,0:0:0:0:", "256,192,100,8,0,-1e40")
        hitobject_tc("256,192,100,8,0,-2147483648,0:0:0:0:", "256,192,100,8,0,NaN")
        hitobject_tc("## PARSE ERROR", "256,192,100,8,0,AAA")
        hitobject_tc("## PARSE ERROR", "256,192,100,8,0,")

    [<Test>]
    let HitObject_ValidParses_Hold_EndTime () =

        hitobject_tc("256,192,100,128,0,100:0:0:0:0:", "256,192,100,128,0,100.1")
        hitobject_tc("256,192,100,128,0,100:0:0:0:0:", "256,192,100,128,0, \t100 ")
        hitobject_tc("256,192,100,128,0,-2147483648:0:0:0:0:", "256,192,100,128,0,1e40")
        hitobject_tc("256,192,100,128,0,-2147483648:0:0:0:0:", "256,192,100,128,0,1e38")
        hitobject_tc("256,192,100,128,0,2147483647:0:0:0:0:", "256,192,100,128,0,2147483648")
        hitobject_tc("256,192,100,128,0,2147483647:0:0:0:0:", "256,192,100,128,0,2147483647")
        hitobject_tc("256,192,100,128,0,-2147483648:0:0:0:0:", "256,192,100,128,0,-2147483648")
        hitobject_tc("256,192,100,128,0,-2147483648:0:0:0:0:", "256,192,100,128,0,-2147483649")
        hitobject_tc("256,192,100,128,0,-2147483648:0:0:0:0:", "256,192,100,128,0,-1e38")
        hitobject_tc("256,192,100,128,0,-2147483648:0:0:0:0:", "256,192,100,128,0,-1e40")
        hitobject_tc("256,192,100,128,0,-2147483648:0:0:0:0:", "256,192,100,128,0,NaN")
        hitobject_tc("## PARSE ERROR", "256,192,100,128,0,AAA")
        hitobject_tc("## PARSE ERROR", "256,192,100,128,0,")

    [<Test>]
    let HitObject_ValidParses_Slider_Curve () =

        hitobject_tc("0,0,1,2,0,B,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,1,140")
        hitobject_tc("0,0,1,2,0,L,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,L,1,140")
        hitobject_tc("0,0,1,2,0,P,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,P,1,140")
        hitobject_tc("0,0,1,2,0,C,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,C,1,140")
        hitobject_tc("0,0,1,2,0,C,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,b,1,140")
        hitobject_tc("0,0,1,2,0,C,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,l,1,140")
        hitobject_tc("0,0,1,2,0,C,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,p,1,140")
        hitobject_tc("0,0,1,2,0,C,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0, ,1,140")
        hitobject_tc("0,0,1,2,0,C,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,\t,1,140")
        hitobject_tc("## PARSE ERROR", "0,0,1,2,0, C,1,140")
        hitobject_tc("## PARSE ERROR", "0,0,1,2,0,C ,1,140")
        hitobject_tc("## PARSE ERROR", "0,0,1,2,0,\tC,1,140")

        // Curve Points
        hitobject_tc("## PARSE ERROR", "0,0,1,2,0,B|,1,140")
        hitobject_tc("## PARSE ERROR", "0,0,1,2,0,B|0,1,140")
        hitobject_tc("## PARSE ERROR", "0,0,1,2,0,B|0:,1,140")
        hitobject_tc("## PARSE ERROR", "0,0,1,2,0,B|:0,1,140")
        hitobject_tc("## PARSE ERROR", "0,0,1,2,0,B|a:0,1,140")
        hitobject_tc("## PARSE ERROR", "0,0,1,2,0,B|0:0|,1,140")
        hitobject_tc("0,0,1,2,0,B|1:2|3:4,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B|1:2|3:4,1,140")
        hitobject_tc("0,0,1,2,0,B|0:1,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B|0.9:1.9,1,140")
        hitobject_tc("0,0,1,2,0,B|0:1,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B|0:1:a,1,140")
        hitobject_tc("0,0,1,2,0,B|0:1,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B|0:1:::::::,1,140")
        hitobject_tc("0,0,1,2,0,B|0:0,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B| 0 : 0 ,1,140")
        hitobject_tc("0,0,1,2,0,B|-2147483648:0,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B|NaN:0,1,140")
        hitobject_tc("0,0,1,2,0,B|-2147483648:0,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B|1e10:0,1,140")
        hitobject_tc("0,0,1,2,0,B|-2147483648:0,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B|-2147483649:0,1,140")
        hitobject_tc("0,0,1,2,0,B|-2147483648:0,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B|2147483584:0,1,140")
        hitobject_tc("0,0,1,2,0,B|2147483520:0,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B|2147483583:0,1,140")
        hitobject_tc("0,0,1,2,0,B|-2147483520:0,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B|-2147483583:0,1,140")
        hitobject_tc("0,0,1,2,0,B|-2147483648:0,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B|-2147483584:0,1,140")
        hitobject_tc("0,0,1,2,0,B|-2147483648:0,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B|-2147483649:0,1,140")

    [<Test>]
    let HitObject_ValidParses_Slider_Slides () =

        hitobject_tc("0,0,1,2,0,B,2,140,0|0|0,0:0|0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,2,140")
        hitobject_tc("0,0,1,2,0,B,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,0,140")
        hitobject_tc("## PARSE ERROR", "0,0,1,2,0,B,9001,140")
        hitobject_tc("## PARSE ERROR", "0,0,1,2,0,B,,140")
        hitobject_tc("## PARSE ERROR", "0,0,1,2,0,B,-2147483649,140")
        hitobject_tc("0,0,1,2,0,B,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,-2147483648,140")

    [<Test>]
    let HitObject_ValidParses_Slider_Length () =

        hitobject_tc("0,0,1,2,0,B,1,70,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,0,0")
        hitobject_tc("## PARSE ERROR", "0,0,1,2,0,B,0,-1")
        hitobject_tc("0,0,1,2,0,B,1,1E-300,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,0,1e-300")
        hitobject_tc("0,0,1,2,0,B,1,70,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,0,1e-400")
        hitobject_tc("0,0,1,2,0,B,1,2.5,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,0,2.5")
        hitobject_tc("0,0,1,2,0,B,1,1000000,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,0,1000000")
        hitobject_tc("## PARSE ERROR", "0,0,1,2,0,B,0,1000001")
        hitobject_tc("0,0,1,2,0,B,1,NaN,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,0,NaN")

    [<Test>]
    let HitObject_ValidParses_Slider_EdgeSounds () =

        hitobject_tc("0,0,1,2,0,B,1,70,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,0,70,")
        hitobject_tc("0,0,1,2,0,B,1,70,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,0,70, ")
        hitobject_tc("0,0,1,2,0,B,1,70,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,0,70,0")
        hitobject_tc("0,0,1,2,0,B,1,70,1|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,0,70,1")
        hitobject_tc("0,0,1,2,0,B,1,70,2|3,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,0,70,  2  |  3  ")
        hitobject_tc("0,0,1,2,0,B,1,70,-2147483648|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,0,70,-2147483648")
        hitobject_tc("0,0,1,2,0,B,1,70,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,0,70,-2147483649")
        hitobject_tc("0,0,1,2,0,B,1,70,2147483647|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,0,70,2147483647")
        hitobject_tc("0,0,1,2,0,B,1,70,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,0,70,2147483648")
        hitobject_tc("0,0,1,2,0,B,1,70,0|5,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,0,70,AAA|5")
        hitobject_tc("0,0,1,2,0,B,1,70,0|5,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,0,70,\t|5")
        hitobject_tc("0,0,1,2,0,B,1,70,0|5,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,0,70, |5")

    [<Test>]
    let HitObject_ValidParses_Slider_EdgeSets () =

        hitobject_tc("0,0,1,2,0,B,1,70,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,0,70,0|0,")
        hitobject_tc("0,0,1,2,0,B,1,70,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,0,70,0|0,   ")
        hitobject_tc("## PARSE ERROR", "0,0,1,2,0,B,0,70,0|0,|")
        hitobject_tc("## PARSE ERROR", "0,0,1,2,0,B,0,70,0|0,0:0|0:0|AAA")
        hitobject_tc("0,0,1,2,0,B,1,70,0|0,2147483647:0|0:0,0:0:0:0:", "0,0,1,2,0,B,0,70,0|0,2147483647:0")
        hitobject_tc("## PARSE ERROR", "0,0,1,2,0,B,0,70,0|0,2147483648:0")
