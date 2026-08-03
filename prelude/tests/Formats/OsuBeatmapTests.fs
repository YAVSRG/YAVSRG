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

    // Therefore Interlude accepts a subset of what osu!stable accepts

    [<Test>]
    let TimingPoint_Defaults () =

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

    let parse_tp (expected: string, input: string) =
        let result =
            try
                TimingPoint.FromString(input).ToString()
            with _ ->
                "## PARSE ERROR"

        Assert.AreEqual(expected, result)

    [<Test>]
    let TimingPoint_ValidParses_Time () =
        parse_tp("## PARSE ERROR", "")
        parse_tp("## PARSE ERROR", "100.1")
        parse_tp("## PARSE ERROR", "100.1,")

        parse_tp("## PARSE ERROR", ",250.1")
        parse_tp("## PARSE ERROR", "AAA,250.1")
        parse_tp("## PARSE ERROR", "NaN,250.1")
        parse_tp("1E+100,250.1,4,2,0,100,1,0", "1e100,250.1")
        parse_tp("-1E+100,250.1,4,2,0,100,1,0", "-1e100,250.1")
        parse_tp("2147483648,250.1,4,2,0,100,1,0", "2147483648,250.1")
        parse_tp("-2147483649,250.1,4,2,0,100,1,0", "-2147483649,250.1")
        parse_tp("-2147483648,250.1,4,2,0,100,1,0", "-2147483648,250.1")
        parse_tp("100.1,250.1,4,2,0,100,1,0", " 100.1 ,250.1")
        parse_tp("2147483647,250,4,2,0,100,1,0", "2147483647,250,4,2,0,100,1,0")

    [<Test>]
    let TimingPoint_ValidParses_MsPerBeat () =
        parse_tp("## PARSE ERROR", "100.1,")
        parse_tp("## PARSE ERROR", "100.1,AAA")
        parse_tp("## PARSE ERROR", "100.1,0")
        // Accepted by osu! but not us
        parse_tp("## PARSE ERROR", "100.1,NaN")
        parse_tp("100.1,1E+100,4,2,0,100,1,0", "100.1,1e100")
        parse_tp("100.1,2147483648,4,2,0,100,1,0", "100.1,2147483648")
        // Negative BeatLength values are treated the same as positive for scroll speed purposes
        parse_tp("100.1,-1E+100,4,2,0,100,1,0", "100.1,-1e100")
        parse_tp("100.1,-1,4,2,0,100,1,0", "100.1,-1")
        parse_tp("100.1,250.1,4,2,0,100,1,0", "100.1, \t250.1 ")

    [<Test>]
    let TimingPoint_ValidParses_Meter () =
        parse_tp("## PARSE ERROR", "100.1,250.1,4")
        parse_tp("## PARSE ERROR", "100.1,250.1,,2,0,100,1,0")
        parse_tp("## PARSE ERROR", "100.1,250.1,1.1,2,0,100,1,0")
        parse_tp("## PARSE ERROR", "100.1,250.1,AAA,2,0,100,1,0")
        parse_tp("## PARSE ERROR", "100.1,250.1,NaN,2,0,100,1,0")
        parse_tp("## PARSE ERROR", "100.1,250.1,1e100,2,0,100,1,0")
        parse_tp("## PARSE ERROR", "100.1,250.1,-1e100,2,0,100,1,0")
        parse_tp("## PARSE ERROR", "100.1,250.1,2147483648,2,0,100,1,0")
        parse_tp("100.1,250.1,2147483647,2,0,100,1,0", "100.1,250.1,2147483647,2,0,100,1,0")
        parse_tp("100.1,250.1,-2147483648,2,0,100,1,0", "100.1,250.1,-2147483648,2,0,100,1,0")
        parse_tp("## PARSE ERROR", "100.1,250.1,-2147483649,2,0,100,1,0")
        parse_tp("100.1,250.1,3,2,0,100,1,0", "100.1,250.1,3,2,0,100,1,0")
        parse_tp("100.1,250.1,3,2,0,100,1,0", "100.1,250.1, 3 ,2,0,100,1,0")
        parse_tp("100.1,250.1,4,2,0,100,1,0", "100.1,250.1,0,2,0,100,1,0")
        parse_tp("100.1,250.1,-1,2,0,100,1,0", "100.1,250.1,-1,2,0,100,1,0")

    [<Test>]
    let TimingPoint_ValidParses_SampleSet () =
        parse_tp("## PARSE ERROR", "100.1,250.1,4,2147483648,0,100,1,0")
        parse_tp("100.1,250.1,4,2147483647,0,100,1,0", "100.1,250.1,4,2147483647,0,100,1,0")
        parse_tp("100.1,250.1,4,-1,0,100,1,0", "100.1,250.1,4,-1,0,100,1,0")
        parse_tp("100.1,250.1,4,-2147483648,0,100,1,0", "100.1,250.1,4,-2147483648,0,100,1,0")
        parse_tp("## PARSE ERROR", "100.1,250.1,4,-2147483649,0,100,1,0")
        parse_tp("## PARSE ERROR", "100.1,250.1,4,,0,100,1,0")
        parse_tp("## PARSE ERROR", "100.1,250.1,4,1.1,0,100,1,0")
        parse_tp("## PARSE ERROR", "100.1,250.1,4,Soft,0,100,1,0")
        parse_tp("100.1,250.1,4,1,0,100,1,0", "100.1,250.1,4,  1  ,0,100,1,0")
        parse_tp("100.1,250.1,4,2,0,100,1,0", "100.1,250.1,4,0,0,100,1,0")

    [<Test>]
    let TimingPoint_ValidParses_SampleIndex () =
        parse_tp("## PARSE ERROR", "100.1,250.1,4,0,2147483648,100,1,0")
        parse_tp("100.1,250.1,4,2,2147483647,100,1,0", "100.1,250.1,4,2,2147483647,100,1,0")
        parse_tp("100.1,250.1,4,2,-1,100,1,0", "100.1,250.1,4,2,-1,100,1,0")
        parse_tp("100.1,250.1,4,2,-2147483648,100,1,0", "100.1,250.1,4,2,-2147483648,100,1,0")
        parse_tp("## PARSE ERROR", "100.1,250.1,4,2,-2147483649,100,1,0")
        parse_tp("## PARSE ERROR", "100.1,250.1,4,2,,100,1,0")
        parse_tp("## PARSE ERROR", "100.1,250.1,4,2,1.1,100,1,0")
        parse_tp("## PARSE ERROR", "100.1,250.1,4,2,AAA,100,1,0")
        parse_tp("100.1,250.1,4,2,1,100,1,0", "100.1,250.1,4,2,  1  ,100,1,0")

    [<Test>]
    let TimingPoint_ValidParses_Volume () =
        parse_tp("100.1,250.1,4,2,0,1,1,0", "100.1,250.1,4,2,0,-1")
        parse_tp("100.1,250.1,4,2,0,1,1,0", "100.1,250.1,4,2,0,0")
        parse_tp("## PARSE ERROR", "100.1,250.1,4,2,0,")
        parse_tp("## PARSE ERROR", "100.1,250.1,4,2,0,AAA")
        parse_tp("## PARSE ERROR", "100.1,250.1,4,2,0,1e100")
        parse_tp("## PARSE ERROR", "100.1,250.1,4,2,0,NaN")
        parse_tp("100.1,250.1,4,2,0,100,1,0", "100.1,250.1,4,2,0,101")
        parse_tp("100.1,250.1,4,2,0,100,1,0", "100.1,250.1,4,2,0,2147483647")
        parse_tp("## PARSE ERROR", "100.1,250.1,4,2,0,2147483648")
        parse_tp("100.1,250.1,4,2,0,50,1,0", "100.1,250.1,4,2,0, 50")

    [<Test>]
    let TimingPoint_ValidParses_Is_Uninherited () =
        // These are uninherited in osu!stable
        parse_tp("100.1,250.1,4,2,0,100,1,0", "100.1,250.1,4,2,0,100,1")
        parse_tp("100.1,250.1,4,2,0,100,1,0", "100.1,250.1,4,2,0,100,100")
        parse_tp("100.1,250.1,4,2,0,100,1,0", "100.1,250.1,4,2,0,100,1A")
        parse_tp("100.1,250.1,4,2,0,100,1,0", "100.1,250.1,4,2,0,100,1.1")
        // There are inherited in osu!stable
        parse_tp("100.1,-100,4,2,0,100,0,0", "100.1,250.1,4,2,0,100,\"1\"")
        // In osu!lazer this test case parses as Uninherited -- todo: bug report?
        parse_tp("100.1,-100,4,2,0,100,0,0", "100.1,250.1,4,2,0,100, 1")
        parse_tp("100.1,-100,4,2,0,100,0,0", "100.1,250.1,4,2,0,100,AAA")
        parse_tp("100.1,-100,4,2,0,100,0,0", "100.1,250.1,4,2,0,100,")
        parse_tp("100.1,-100,4,2,0,100,0,0", "100.1,250.1,4,2,0,100,0")
        parse_tp("100.1,-100,4,2,0,100,0,0", "100.1,250.1,4,2,0,100,2")
        parse_tp("100.1,-100,4,2,0,100,0,0", "100.1,250.1,4,2,0,100,-1")
        parse_tp("## PARSE ERROR", "100.1,-100,EVEN_THOUGH_IGNORED,2,0,100,0")

    [<Test>]
    let TimingPoint_ValidParses_Inherited_Multiplier () =
        parse_tp("100.1,-50,4,2,0,100,0,0", "100.1,-50,4,2,0,100,0")
        parse_tp("100.1,-1,4,2,0,100,0,0", "100.1,-0.1,4,2,0,100,0")
        parse_tp("100.1,-10000,4,2,0,100,0,0", "100.1,-100000,4,2,0,100,0")
        parse_tp("100.1,-100,4,2,0,100,0,0", "100.1,0,4,2,0,100,0")
        // osu! accepts it verbatim as NaN but it acts like -100
        parse_tp("100.1,-100,4,2,0,100,0,0", "100.1,NaN,4,2,0,100,0")
        parse_tp("100.1,-100,4,2,0,100,0,0", "100.1,1e100,4,2,0,100,0")
        parse_tp("100.1,-1,4,2,0,100,0,0", "100.1,-1e-100,4,2,0,100,0")
        parse_tp("100.1,-10000,4,2,0,100,0,0", "100.1,-1e100,4,2,0,100,0")

    [<Test>]
    let TimingPoint_ValidParses_Effects () =
        parse_tp("## PARSE ERROR", "100.1,250.1,4,2,0,100,1,-2147483649")
        parse_tp("## PARSE ERROR", "100.1,250.1,4,2,0,100,1,2147483648")
        parse_tp("100.1,250.1,4,2,0,100,1,-2147483648", "100.1,250.1,4,2,0,100,1,-2147483648")
        parse_tp("100.1,250.1,4,2,0,100,1,2147483647", "100.1,250.1,4,2,0,100,1,2147483647")
        parse_tp("## PARSE ERROR", "100.1,250.1,4,2,0,100,1,")
        parse_tp("## PARSE ERROR", "100.1,250.1,4,2,0,100,1,AAA")
        parse_tp("## PARSE ERROR", "100.1,250.1,4,2,0,100,1,1.1")
        parse_tp("100.1,250.1,4,2,0,100,1,1", "100.1,250.1,4,2,0,100,1, 1")

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

    let parse_hobj (expected: string, input: string) =
        let result =
            try
                HitObject.FromString(input).ToString()
            with _ ->
                "## PARSE ERROR"

        Assert.AreEqual(expected, result)

    [<Test>]
    let HitObject_ValidParses () =

        parse_hobj("## PARSE ERROR", " 256,192,0,1,0")
        parse_hobj("256,192,0,1,0,0:0:0:0:", "256 , 192 , 0 , 1 , 0")
        parse_hobj("256,192,0,1,0,0:0:0:0:", "\t256\t,\t192\t,\t0\t,\t1,\t0\t")
        parse_hobj("## PARSE ERROR", "256,192,0,1,0,")
        parse_hobj("## PARSE ERROR", "256,192,0,1,0,0:0:0:0")
        parse_hobj("256,192,0,1,0,1:1:1:1:", "256,192,0,1,0,1:1:1:1:")

    [<Test>]
    let HitObject_ValidParses_X () =

        parse_hobj("256,192,0,1,0,0:0:0:0:", "256.9,192,0,1,0")
        parse_hobj("## PARSE ERROR", "1e40,192,0,1,0")
        parse_hobj("512,192,0,1,0,0:0:0:0:", "1e38,192,0,1,0")
        parse_hobj("512,192,0,1,0,0:0:0:0:", "2147483648,192,0,1,0")
        parse_hobj("512,192,0,1,0,0:0:0:0:", "2147483647,192,0,1,0")
        parse_hobj("0,192,0,1,0,0:0:0:0:", "-2147483648,192,0,1,0")
        parse_hobj("0,192,0,1,0,0:0:0:0:", "-2147483649,192,0,1,0")
        parse_hobj("0,192,0,1,0,0:0:0:0:", "-1e38,192,0,1,0")
        parse_hobj("## PARSE ERROR", "-1e40,192,0,1,0")
        parse_hobj("## PARSE ERROR", "NaN,192,0,1,0")
        parse_hobj("## PARSE ERROR", "AAA,192,0,1,0")
        parse_hobj("## PARSE ERROR", ",192,0,1,0")

    [<Test>]
    let HitObject_ValidParses_Y () =

        parse_hobj("256,192,0,1,0,0:0:0:0:", "256,192.9,0,1,0")
        parse_hobj("## PARSE ERROR", "256,1e40,0,1,0")
        parse_hobj("256,512,0,1,0,0:0:0:0:", "256,1e38,0,1,0")
        parse_hobj("256,512,0,1,0,0:0:0:0:", "256,2147483648,0,1,0")
        parse_hobj("256,512,0,1,0,0:0:0:0:", "256,2147483647,0,1,0")
        parse_hobj("256,0,0,1,0,0:0:0:0:", "256,-2147483648,0,1,0")
        parse_hobj("256,0,0,1,0,0:0:0:0:", "256,-2147483649,0,1,0")
        parse_hobj("256,0,0,1,0,0:0:0:0:", "256,-1e38,0,1,0")
        parse_hobj("## PARSE ERROR", "256,-1e40,0,1,0")
        parse_hobj("## PARSE ERROR", "256,NaN,0,1,0")
        parse_hobj("## PARSE ERROR", "256,AAA,0,1,0")
        parse_hobj("## PARSE ERROR", "256,,0,1,0")

    [<Test>]
    let HitObject_ValidParses_Time () =

        parse_hobj("256,192,100,1,0,0:0:0:0:", "256,192,100.1,1,0")
        parse_hobj("## PARSE ERROR", "256,192,1e40,1,0")
        parse_hobj("## PARSE ERROR", "256,192,1e38,1,0")
        parse_hobj("256,192,2147483647,1,0,0:0:0:0:", "256,192,2147483648,1,0")
        parse_hobj("256,192,2147483647,1,0,0:0:0:0:", "256,192,2147483647,1,0")
        // osu! will strip out notes that are beyond the end of the audio file, and objects with negative timestamps
        // It does seem to load them though, just refuses to save them again, so I opted for parse error for negatives
        parse_hobj("## PARSE ERROR", "256,192,-2147483648,1,0")
        // Same as case above
        parse_hobj("## PARSE ERROR", "256,192,-2147483649,1,0")
        parse_hobj("## PARSE ERROR", "256,192,-1e38,1,0")
        parse_hobj("## PARSE ERROR", "256,192,-1e40,1,0")
        parse_hobj("## PARSE ERROR", "256,192,NaN,1,0")
        parse_hobj("## PARSE ERROR", "256,192,AAA,1,0")
        parse_hobj("## PARSE ERROR", "256,192,,1,0")

    [<Test>]
    let HitObject_ValidParses_Type () =

        parse_hobj("## PARSE ERROR", "256,192,100,0,0")
        parse_hobj("## PARSE ERROR", "256,192,100,1.0,0")
        parse_hobj("## PARSE ERROR", "256,192,100,,0")
        parse_hobj("## PARSE ERROR", "256,192,100,AAA,0")

        parse_hobj("256,192,100,1,0,0:0:0:0:", "256,192,100,1,0")
        parse_hobj("256,192,100,1,0,0:0:0:0:", "256,192,100,3,0")
        parse_hobj("256,192,100,1,0,0:0:0:0:", "256,192,100,9,0")
        parse_hobj("256,192,100,1,0,0:0:0:0:", "256,192,100,129,0")
        parse_hobj("256,192,100,1,0,0:0:0:0:", "256,192,100,137,0")
        parse_hobj("256,192,100,1,0,0:0:0:0:", "256,192,100,139,0")
        parse_hobj("256,192,100,5,0,0:0:0:0:", "256,192,100,5,0")
        parse_hobj("256,192,100,5,0,0:0:0:0:", "256,192,100,13,0")
        parse_hobj("256,192,100,5,0,0:0:0:0:", "256,192,100,133,0")

    [<Test>]
    let HitObject_ValidParses_Spinner_EndTime () =

        parse_hobj("256,192,100,8,0,100,0:0:0:0:", "256,192,100,8,0,100.1")
        parse_hobj("256,192,100,8,0,100,0:0:0:0:", "256,192,100,8,0, \t100 ")
        parse_hobj("256,192,100,8,0,-2147483648,0:0:0:0:", "256,192,100,8,0,1e40")
        parse_hobj("256,192,100,8,0,-2147483648,0:0:0:0:", "256,192,100,8,0,1e38")
        parse_hobj("256,192,100,8,0,2147483647,0:0:0:0:", "256,192,100,8,0,2147483648")
        parse_hobj("256,192,100,8,0,2147483647,0:0:0:0:", "256,192,100,8,0,2147483647")
        parse_hobj("256,192,100,8,0,-2147483648,0:0:0:0:", "256,192,100,8,0,-2147483648")
        parse_hobj("256,192,100,8,0,-2147483648,0:0:0:0:", "256,192,100,8,0,-2147483649")
        parse_hobj("256,192,100,8,0,-2147483648,0:0:0:0:", "256,192,100,8,0,-1e38")
        parse_hobj("256,192,100,8,0,-2147483648,0:0:0:0:", "256,192,100,8,0,-1e40")
        parse_hobj("256,192,100,8,0,-2147483648,0:0:0:0:", "256,192,100,8,0,NaN")
        parse_hobj("## PARSE ERROR", "256,192,100,8,0,AAA")
        parse_hobj("## PARSE ERROR", "256,192,100,8,0,")

    [<Test>]
    let HitObject_ValidParses_Hold_EndTime () =

        parse_hobj("256,192,100,128,0,100:0:0:0:0:", "256,192,100,128,0,100.1")
        parse_hobj("256,192,100,128,0,100:0:0:0:0:", "256,192,100,128,0, \t100 ")
        parse_hobj("256,192,100,128,0,-2147483648:0:0:0:0:", "256,192,100,128,0,1e40")
        parse_hobj("256,192,100,128,0,-2147483648:0:0:0:0:", "256,192,100,128,0,1e38")
        parse_hobj("256,192,100,128,0,2147483647:0:0:0:0:", "256,192,100,128,0,2147483648")
        parse_hobj("256,192,100,128,0,2147483647:0:0:0:0:", "256,192,100,128,0,2147483647")
        parse_hobj("256,192,100,128,0,-2147483648:0:0:0:0:", "256,192,100,128,0,-2147483648")
        parse_hobj("256,192,100,128,0,-2147483648:0:0:0:0:", "256,192,100,128,0,-2147483649")
        parse_hobj("256,192,100,128,0,-2147483648:0:0:0:0:", "256,192,100,128,0,-1e38")
        parse_hobj("256,192,100,128,0,-2147483648:0:0:0:0:", "256,192,100,128,0,-1e40")
        parse_hobj("256,192,100,128,0,-2147483648:0:0:0:0:", "256,192,100,128,0,NaN")
        parse_hobj("## PARSE ERROR", "256,192,100,128,0,AAA")
        parse_hobj("## PARSE ERROR", "256,192,100,128,0,")

    [<Test>]
    let HitObject_ValidParses_Slider_Curve () =

        parse_hobj("0,0,1,2,0,B,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,1,140")
        parse_hobj("0,0,1,2,0,L,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,L,1,140")
        parse_hobj("0,0,1,2,0,P,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,P,1,140")
        parse_hobj("0,0,1,2,0,C,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,C,1,140")
        parse_hobj("0,0,1,2,0,C,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,b,1,140")
        parse_hobj("0,0,1,2,0,C,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,l,1,140")
        parse_hobj("0,0,1,2,0,C,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,p,1,140")
        parse_hobj("0,0,1,2,0,C,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0, ,1,140")
        parse_hobj("0,0,1,2,0,C,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,\t,1,140")
        parse_hobj("## PARSE ERROR", "0,0,1,2,0, C,1,140")
        parse_hobj("## PARSE ERROR", "0,0,1,2,0,C ,1,140")
        parse_hobj("## PARSE ERROR", "0,0,1,2,0,\tC,1,140")

        parse_hobj("## PARSE ERROR", "0,0,1,2,0,B|,1,140")
        parse_hobj("## PARSE ERROR", "0,0,1,2,0,B|0,1,140")
        parse_hobj("## PARSE ERROR", "0,0,1,2,0,B|0:,1,140")
        parse_hobj("## PARSE ERROR", "0,0,1,2,0,B|:0,1,140")
        parse_hobj("## PARSE ERROR", "0,0,1,2,0,B|a:0,1,140")
        parse_hobj("## PARSE ERROR", "0,0,1,2,0,B|0:0|,1,140")
        parse_hobj("0,0,1,2,0,B|1:2|3:4,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B|1:2|3:4,1,140")
        parse_hobj("0,0,1,2,0,B|0:1,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B|0.9:1.9,1,140")
        parse_hobj("0,0,1,2,0,B|0:1,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B|0:1:a,1,140")
        parse_hobj("0,0,1,2,0,B|0:1,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B|0:1:::::::,1,140")
        parse_hobj("0,0,1,2,0,B|0:0,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B| 0 : 0 ,1,140")
        parse_hobj("0,0,1,2,0,B|-2147483648:0,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B|NaN:0,1,140")
        parse_hobj("0,0,1,2,0,B|-2147483648:0,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B|1e10:0,1,140")
        parse_hobj("0,0,1,2,0,B|-2147483648:0,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B|-2147483649:0,1,140")
        parse_hobj("0,0,1,2,0,B|-2147483648:0,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B|2147483584:0,1,140")
        parse_hobj("0,0,1,2,0,B|2147483520:0,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B|2147483583:0,1,140")
        parse_hobj("0,0,1,2,0,B|-2147483520:0,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B|-2147483583:0,1,140")
        parse_hobj("0,0,1,2,0,B|-2147483648:0,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B|-2147483584:0,1,140")
        parse_hobj("0,0,1,2,0,B|-2147483648:0,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B|-2147483649:0,1,140")

    [<Test>]
    let HitObject_ValidParses_Slider_Slides () =

        parse_hobj("0,0,1,2,0,B,2,140,0|0|0,0:0|0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,2,140")
        parse_hobj("0,0,1,2,0,B,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,0,140")
        parse_hobj("## PARSE ERROR", "0,0,1,2,0,B,9001,140")
        parse_hobj("## PARSE ERROR", "0,0,1,2,0,B,,140")
        parse_hobj("## PARSE ERROR", "0,0,1,2,0,B,-2147483649,140")
        parse_hobj("0,0,1,2,0,B,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,-2147483648,140")

    [<Test>]
    let HitObject_ValidParses_Slider_Length () =

        parse_hobj("0,0,1,2,0,B,1,70,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,0,0")
        parse_hobj("## PARSE ERROR", "0,0,1,2,0,B,0,-1")
        parse_hobj("0,0,1,2,0,B,1,1E-300,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,0,1e-300")
        parse_hobj("0,0,1,2,0,B,1,70,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,0,1e-400")
        parse_hobj("0,0,1,2,0,B,1,2.5,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,0,2.5")
        parse_hobj("0,0,1,2,0,B,1,1000000,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,0,1000000")
        parse_hobj("## PARSE ERROR", "0,0,1,2,0,B,0,1000001")
        // Yes osu! really accepts this, I will too since Interlude doesn't work with sliders anyway
        parse_hobj("0,0,1,2,0,B,1,NaN,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,0,NaN")

    [<Test>]
    let HitObject_ValidParses_Slider_EdgeSounds () =

        parse_hobj("0,0,1,2,0,B,1,70,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,0,70,")
        parse_hobj("0,0,1,2,0,B,1,70,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,0,70, ")
        parse_hobj("0,0,1,2,0,B,1,70,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,0,70,0")
        parse_hobj("0,0,1,2,0,B,1,70,1|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,0,70,1")
        parse_hobj("0,0,1,2,0,B,1,70,2|3,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,0,70,  2  |  3  ")
        parse_hobj("0,0,1,2,0,B,1,70,-2147483648|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,0,70,-2147483648")
        parse_hobj("0,0,1,2,0,B,1,70,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,0,70,-2147483649")
        parse_hobj("0,0,1,2,0,B,1,70,2147483647|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,0,70,2147483647")
        parse_hobj("0,0,1,2,0,B,1,70,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,0,70,2147483648")
        parse_hobj("0,0,1,2,0,B,1,70,0|5,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,0,70,AAA|5")
        parse_hobj("0,0,1,2,0,B,1,70,0|5,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,0,70,\t|5")
        parse_hobj("0,0,1,2,0,B,1,70,0|5,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,0,70, |5")

    [<Test>]
    let HitObject_ValidParses_Slider_EdgeSets () =

        parse_hobj("0,0,1,2,0,B,1,70,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,0,70,0|0,")
        parse_hobj("0,0,1,2,0,B,1,70,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,0,70,0|0,   ")
        parse_hobj("## PARSE ERROR", "0,0,1,2,0,B,0,70,0|0,|")
        // Yes osu! parses all pairs even when not used based on the number of slides
        parse_hobj("## PARSE ERROR", "0,0,1,2,0,B,0,70,0|0,0:0|0:0|AAA")
        parse_hobj("0,0,1,2,0,B,1,70,0|0,2147483647:0|0:0,0:0:0:0:", "0,0,1,2,0,B,0,70,0|0,2147483647:0")
        parse_hobj("## PARSE ERROR", "0,0,1,2,0,B,0,70,0|0,2147483648:0")

    let parse_evt (expected: string, input: string) =
        let result =
            try
                match StoryboardEvent.TryParse(input) with
                | Some v -> v.ToString()
                | None -> "## UNSUPPORTED"
            with _ ->
                "## PARSE ERROR"

        Assert.AreEqual(expected, result)

    [<Test>]
    let StoryboardEvent_ValidParses_Background () =

        parse_evt("""0,0,"bg.png",0,0""", "0,0,\"bg.png\",0,0")
        parse_evt("""0,0,"bg.png",0,0""", "0,0,bg.png,0,0")
        parse_evt("""0,0,"bg.png",0,0""", "0,0,bg.png")
        parse_evt("## UNSUPPORTED", " 0,0,\"bg.png\",0,0")
        parse_evt("## UNSUPPORTED", "_0,0,\"bg.png\",0,0")
        parse_evt("""0,0,"bg.png",0,0""", "\t0,0,bg.png,0,0")
        // osu! seems to use Int32.Parse and then write it back as a Single, for whatever reason
        //parse_evt("""0,0,"bg.png",-2.147484E+09,2.147484E+09""", "0,0,bg.png,-2147483648,2147483647")
        parse_evt("""0,0,"bg.png",-2147483648,2147483647""", "0,0,bg.png,-2147483648,2147483647")
        parse_evt("## PARSE ERROR", "0,0,bg.png,-2147483649,2147483647")
        parse_evt("## PARSE ERROR", "0,0,bg.png,-2147483648,2147483648")
        parse_evt("""0,0,"bg.png",0,0""", "Background,0,bg.png,0,0")
        parse_evt("""0,0,"bg.png",0,0""", "\tBackground,0,bg.png,0,0")
        parse_evt("""0,0,"bg.png",0,0""", "\tBackground\t,0,bg.png,0,0")
        parse_evt("## PARSE ERROR", "background,0,bg.png,0,0")
        // osu! accepts this
        parse_evt("## PARSE ERROR", "00,0,bg.png,0,0")
        // This thing has to parse as an Int32 but is discarded
        parse_evt("""0,0,"bg.png",0,0""", "0,-2147483648,bg.png,0,0")
        parse_evt("## PARSE ERROR", "0,-2147483649,bg.png,0,0")
