namespace Prelude.Tests.Formats

open NUnit.Framework
open Prelude.Formats.Osu

module TimingPointTests =

    // In general, do what osu!stable would do if it parsed this input, and then wrote it back to an .osu file
    // to verify, I put test cases into an .osu file, hit CTRL-L in the editor, then save and see what gets written

    // ## PARSE ERROR = Error on load, Line removed from .osu on save, or File is rendered unplayable
    // OR I have decided to do something different for sanity, if so this is commented

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
