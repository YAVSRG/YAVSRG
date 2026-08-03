namespace Prelude.Tests.Formats

open NUnit.Framework
open Prelude.Formats.Osu

module HitObjectTests =

    // In general, do what osu!stable would do if it parsed this input, and then wrote it back to an .osu file
    // to verify, I put test cases into an .osu file, hit CTRL-L in the editor, then save and see what gets written

    // ## PARSE ERROR = Error on load, Line removed from .osu on save, or File is rendered unplayable
    // OR I have decided to do something different for sanity, if so this is commented

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