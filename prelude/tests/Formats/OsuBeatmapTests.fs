namespace Prelude.Tests.Formats

open NUnit.Framework
open Prelude.Formats.Osu

module OsuBeatmapTests =

    // All test cases are intended to match osu!stable when tested experimentally
    // Each test case represents what osu! would do if it parsed this input, and then wrote it back to an .osu file
    // Try pasting these test cases into your .osu files, hitting CTRL-L in the editor, then CTRL-S and seeing what the client writes back
    
    // ## PARSE ERROR indicates that osu! editor reports an error on load for this input,
    // OR in certain cases I have make a choice to reject:
    // a) osu! will refuse to play a file containing data loaded like this (try F5 in the editor)
    // b) osu! will skip over/ignore this line on load and it is removed when writing back
    
    [<Test>]
    let TimingPoint_ValidParses () =

        let inline expected_result (expected: string, input: string) =
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
                    MsPerBeat = 500
                    Meter = 4
                    SampleSet = SampleSet.Default
                    SampleIndex = 0
                    Volume = 100
                    Effects = TimingEffect.None
                }
            )

        Assert.AreEqual(default_values, TimingPoint.FromString("100.1,"))


        expected_result("## PARSE ERROR", "")
        expected_result("## PARSE ERROR", "100.1")

        // Time
        expected_result("## PARSE ERROR", ",250.1")
        expected_result("## PARSE ERROR", "AAA,250.1")
        expected_result("## PARSE ERROR", "NaN,250.1")
        expected_result("## PARSE ERROR", "1e100,250.1")
        expected_result("## PARSE ERROR", "-1e100,250.1")
        expected_result("## PARSE ERROR", "2147483648,250.1")
        expected_result("## PARSE ERROR", "-2147483649,250.1")
        expected_result("-2147483648,250.1,4,0,0,100,1,0", "-2147483648,250.1")
        expected_result("100.1,250.1,4,0,0,100,1,0", " 100.1,250.1")

        // MsPerBeat
        expected_result("100.1,500,4,0,0,100,1,0", "100.1,")
        expected_result("100.1,500,4,0,0,100,1,0", "100.1,AAA")
        expected_result("100.1,500,4,0,0,100,1,0", "100.1,NaN")
        expected_result("100.1,1E+100,4,0,0,100,1,0", "100.1,1e100")
        expected_result("100.1,2147483648,4,0,0,100,1,0", "100.1,2147483648")
        expected_result("100.1,500,4,0,0,100,1,0", "100.1,-1e100")
        expected_result("100.1,500,4,0,0,100,1,0", "100.1,-1")
        expected_result("100.1,500,4,0,0,100,1,0", "100.1,0")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1, 250.1")

        // Meter
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,0")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,-1")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,AAA")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,NaN")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,1e100")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,-1e100")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,2147483648")
        expected_result("100.1,250.1,2147483647,0,0,100,1,0", "100.1,250.1,2147483647")
        expected_result("100.1,250.1,3,0,0,100,1,0", "100.1,250.1,3")
        expected_result("100.1,250.1,3,0,0,100,1,0", "100.1,250.1, 3")

        // SampleSet
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,-1")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,4")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,AAA")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,1.1")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,Default")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,None")
        expected_result("100.1,250.1,4,1,0,100,1,0", "100.1,250.1,4,Normal")
        expected_result("100.1,250.1,4,2,0,100,1,0", "100.1,250.1,4,Soft")
        expected_result("100.1,250.1,4,3,0,100,1,0", "100.1,250.1,4,Drum")
        expected_result("100.1,250.1,4,1,0,100,1,0", "100.1,250.1,4, 1")

        // SampleIndex
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,0,")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,0,AAA")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,0,-2147483649")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,0,2147483648")
        expected_result("100.1,250.1,4,0,-2147483648,100,1,0", "100.1,250.1,4,0,-2147483648")
        expected_result("100.1,250.1,4,0,2147483647,100,1,0", "100.1,250.1,4,0,2147483647")
        expected_result("100.1,250.1,4,0,2,100,1,0", "100.1,250.1,4,0, 2")

        // Volume
        expected_result("100.1,250.1,4,0,0,0,1,0", "100.1,250.1,4,0,0,-1")
        expected_result("100.1,250.1,4,0,0,0,1,0", "100.1,250.1,4,0,0,0")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,0,0,")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,0,0,AAA")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,0,0,1e100")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,0,0,-1e100")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,0,0,NaN")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,0,0,101")
        expected_result("100.1,250.1,4,0,0,50,1,0", "100.1,250.1,4,0,0, 50")

        // Uninherited
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,0,0,100,1")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,0,0,100,100")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,0,0,100,1A")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,0,0,100,1.1")
        // Inherited
        // In osu!lazer this test case parses as Uninherited -- todo: bug report?
        expected_result("100.1,-100,4,0,0,100,0,0", "100.1,250.1,4,0,0,100, 1")
        expected_result("100.1,-100,4,0,0,100,0,0", "100.1,250.1,4,0,0,100,\"1\"")
        expected_result("100.1,-100,4,0,0,100,0,0", "100.1,250.1,4,0,0,100,AAA")
        expected_result("100.1,-100,4,0,0,100,0,0", "100.1,250.1,4,0,0,100,")
        expected_result("100.1,-100,4,0,0,100,0,0", "100.1,250.1,4,0,0,100,0")
        expected_result("100.1,-100,4,0,0,100,0,0", "100.1,250.1,4,0,0,100,2")
        expected_result("100.1,-100,4,0,0,100,0,0", "100.1,250.1,4,0,0,100,-1")
        // Inherited: Multiplier
        expected_result("100.1,-50,4,0,0,100,0,0", "100.1,-50,4,0,0,100,0")
        expected_result("100.1,-1,4,0,0,100,0,0", "100.1,-0.1,4,0,0,100,0")
        expected_result("100.1,-10000,4,0,0,100,0,0", "100.1,-100000,4,0,0,100,0")
        expected_result("100.1,-100,4,0,0,100,0,0", "100.1,0,4,0,0,100,0")
        expected_result("100.1,-100,4,0,0,100,0,0", "100.1,NaN,4,0,0,100,0")
        expected_result("100.1,-100,4,0,0,100,0,0", "100.1,1e100,4,0,0,100,0")
        expected_result("100.1,-1,4,0,0,100,0,0", "100.1,-1e-100,4,0,0,100,0")
        expected_result("100.1,-10000,4,0,0,100,0,0", "100.1,-1e100,4,0,0,100,0")
        
        // Effects
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,0,0,100,1,-2147483649")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,0,0,100,1,2147483648")
        expected_result("100.1,250.1,4,0,0,100,1,-2147483648", "100.1,250.1,4,0,0,100,1,-2147483648")
        expected_result("100.1,250.1,4,0,0,100,1,2147483647", "100.1,250.1,4,0,0,100,1,2147483647")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,0,0,100,1,")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,0,0,100,1,AAA")
        expected_result("100.1,250.1,4,0,0,100,1,0", "100.1,250.1,4,0,0,100,1,1.1")
        expected_result("100.1,250.1,4,0,0,100,1,1", "100.1,250.1,4,0,0,100,1, 1")
        
    [<Test>]
    let HitSample_ValidParses () =

        let inline expected_result (expected: string, input: string) =
            let result =
                try
                    HitSample.FromString(input).ToString()
                with _ ->
                    "## PARSE ERROR"
                    
            Assert.AreEqual(expected, result)

        expected_result("## PARSE ERROR", "")
        expected_result("## PARSE ERROR", " ")
        expected_result("0:0:0:0:", " 0 :  0  :   0   :    0    :")
        expected_result("0:0:0:0:", "00:00:00:00:")
        expected_result("-1:5:-2147483648:100: File", "-1 : 5 : -2147483648 : 101 : File")
        
        expected_result("## PARSE ERROR", "0:0:0:-2147483649:")
        expected_result("## PARSE ERROR", "0:0:0:2147483648:")
        expected_result("0:0:0:0:", "0:0:0:-2147483648:")
        expected_result("0:0:0:100:", "0:0:0:2147483647:")
        
    [<Test>]
    let HitObject_ValidParses () =
        
        let inline expected_result (expected: string, input: string) =
            let result =
                try
                    HitObject.FromString(input).ToString()
                with _ ->
                    "## PARSE ERROR"
                    
            Assert.AreEqual(expected, result)
            
        expected_result("## PARSE ERROR", " 256,192,0,1,0")
        expected_result("256,192,0,1,0,0:0:0:0:", "256 , 192 , 0 , 1 , 0")
        expected_result("256,192,0,1,0,0:0:0:0:", "\t256\t,\t192\t,\t0\t,\t1,\t0\t")
        expected_result("## PARSE ERROR", "256,192,0,1,0,")
        expected_result("## PARSE ERROR", "256,192,0,1,0,0:0:0:0")
        expected_result("256,192,0,1,0,1:1:1:1:", "256,192,0,1,0,1:1:1:1:")
        
        // X
        expected_result("256,192,0,1,0,0:0:0:0:", "256.9,192,0,1,0")
        expected_result("## PARSE ERROR", "1e40,192,0,1,0")
        expected_result("512,192,0,1,0,0:0:0:0:", "1e38,192,0,1,0")
        expected_result("512,192,0,1,0,0:0:0:0:", "2147483648,192,0,1,0")
        expected_result("512,192,0,1,0,0:0:0:0:", "2147483647,192,0,1,0")
        expected_result("0,192,0,1,0,0:0:0:0:", "-2147483648,192,0,1,0")
        expected_result("0,192,0,1,0,0:0:0:0:", "-2147483649,192,0,1,0")
        expected_result("0,192,0,1,0,0:0:0:0:", "-1e38,192,0,1,0")
        expected_result("## PARSE ERROR", "-1e40,192,0,1,0")
        expected_result("## PARSE ERROR", "NaN,192,0,1,0")
        expected_result("## PARSE ERROR", "AAA,192,0,1,0")
        expected_result("## PARSE ERROR", ",192,0,1,0")
        
        // Y
        expected_result("256,192,0,1,0,0:0:0:0:", "256,192.9,0,1,0")
        expected_result("## PARSE ERROR", "256,1e40,0,1,0")
        expected_result("256,512,0,1,0,0:0:0:0:", "256,1e38,0,1,0")
        expected_result("256,512,0,1,0,0:0:0:0:", "256,2147483648,0,1,0")
        expected_result("256,512,0,1,0,0:0:0:0:", "256,2147483647,0,1,0")
        expected_result("256,0,0,1,0,0:0:0:0:", "256,-2147483648,0,1,0")
        expected_result("256,0,0,1,0,0:0:0:0:", "256,-2147483649,0,1,0")
        expected_result("256,0,0,1,0,0:0:0:0:", "256,-1e38,0,1,0")
        expected_result("## PARSE ERROR", "256,-1e40,0,1,0")
        expected_result("## PARSE ERROR", "256,NaN,0,1,0")
        expected_result("## PARSE ERROR", "256,AAA,0,1,0")
        expected_result("## PARSE ERROR", "256,,0,1,0")
        
        // Time
        expected_result("256,192,100,1,0,0:0:0:0:", "256,192,100.1,1,0")
        expected_result("## PARSE ERROR", "256,192,1e40,1,0")
        expected_result("## PARSE ERROR", "256,192,1e38,1,0")
        // osu! will strip out notes that are beyond the end of the audio file or have negative timestamps
        // Interlude will not but would reject them later as part of conversion
        // Therefore no real way to verify what is intended behaviour here
        expected_result("256,192,2147483647,1,0,0:0:0:0:", "256,192,2147483648,1,0")
        expected_result("256,192,2147483647,1,0,0:0:0:0:", "256,192,2147483647,1,0")
        expected_result("256,192,-2147483648,1,0,0:0:0:0:", "256,192,-2147483648,1,0")
        expected_result("256,192,-2147483648,1,0,0:0:0:0:", "256,192,-2147483649,1,0")
        expected_result("## PARSE ERROR", "256,192,-1e38,1,0")
        expected_result("## PARSE ERROR", "256,192,-1e40,1,0")
        expected_result("## PARSE ERROR", "256,192,NaN,1,0")
        expected_result("## PARSE ERROR", "256,192,AAA,1,0")
        expected_result("## PARSE ERROR", "256,192,,1,0")
        
        // Type
        expected_result("## PARSE ERROR", "256,192,100,0,0")
        expected_result("## PARSE ERROR", "256,192,100,1.0,0")
        expected_result("## PARSE ERROR", "256,192,100,,0")
        expected_result("## PARSE ERROR", "256,192,100,AAA,0")
        
        expected_result("256,192,100,1,0,0:0:0:0:", "256,192,100,1,0")
        expected_result("256,192,100,1,0,0:0:0:0:", "256,192,100,3,0")
        expected_result("256,192,100,1,0,0:0:0:0:", "256,192,100,9,0")
        expected_result("256,192,100,1,0,0:0:0:0:", "256,192,100,129,0")
        expected_result("256,192,100,1,0,0:0:0:0:", "256,192,100,137,0")
        expected_result("256,192,100,1,0,0:0:0:0:", "256,192,100,139,0")
        expected_result("256,192,100,5,0,0:0:0:0:", "256,192,100,5,0")
        expected_result("256,192,100,5,0,0:0:0:0:", "256,192,100,13,0")
        expected_result("256,192,100,5,0,0:0:0:0:", "256,192,100,133,0")
        
        // Spinner: EndTime
        expected_result("256,192,100,8,0,100,0:0:0:0:", "256,192,100,8,0,100.1")
        expected_result("256,192,100,8,0,100,0:0:0:0:", "256,192,100,8,0, \t100 ")
        expected_result("256,192,100,8,0,-2147483648,0:0:0:0:", "256,192,100,8,0,1e40")
        expected_result("256,192,100,8,0,-2147483648,0:0:0:0:", "256,192,100,8,0,1e38")
        expected_result("256,192,100,8,0,2147483647,0:0:0:0:", "256,192,100,8,0,2147483648")
        expected_result("256,192,100,8,0,2147483647,0:0:0:0:", "256,192,100,8,0,2147483647")
        expected_result("256,192,100,8,0,-2147483648,0:0:0:0:", "256,192,100,8,0,-2147483648")
        expected_result("256,192,100,8,0,-2147483648,0:0:0:0:", "256,192,100,8,0,-2147483649")
        expected_result("256,192,100,8,0,-2147483648,0:0:0:0:", "256,192,100,8,0,-1e38")
        expected_result("256,192,100,8,0,-2147483648,0:0:0:0:", "256,192,100,8,0,-1e40")
        expected_result("256,192,100,8,0,-2147483648,0:0:0:0:", "256,192,100,8,0,NaN")
        expected_result("## PARSE ERROR", "256,192,100,8,0,AAA")
        expected_result("## PARSE ERROR", "256,192,100,8,0,")
        
        // Hold: EndTime
        expected_result("256,192,100,128,0,100:0:0:0:0:", "256,192,100,128,0,100.1")
        expected_result("256,192,100,128,0,100:0:0:0:0:", "256,192,100,128,0, \t100 ")
        expected_result("256,192,100,128,0,-2147483648:0:0:0:0:", "256,192,100,128,0,1e40")
        expected_result("256,192,100,128,0,-2147483648:0:0:0:0:", "256,192,100,128,0,1e38")
        expected_result("256,192,100,128,0,2147483647:0:0:0:0:", "256,192,100,128,0,2147483648")
        expected_result("256,192,100,128,0,2147483647:0:0:0:0:", "256,192,100,128,0,2147483647")
        expected_result("256,192,100,128,0,-2147483648:0:0:0:0:", "256,192,100,128,0,-2147483648")
        expected_result("256,192,100,128,0,-2147483648:0:0:0:0:", "256,192,100,128,0,-2147483649")
        expected_result("256,192,100,128,0,-2147483648:0:0:0:0:", "256,192,100,128,0,-1e38")
        expected_result("256,192,100,128,0,-2147483648:0:0:0:0:", "256,192,100,128,0,-1e40")
        expected_result("256,192,100,128,0,-2147483648:0:0:0:0:", "256,192,100,128,0,NaN")
        expected_result("## PARSE ERROR", "256,192,100,128,0,AAA")
        expected_result("## PARSE ERROR", "256,192,100,128,0,")
        
        // Slider: Curve Shape
        expected_result("0,0,1,2,0,B,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B,1,140")
        expected_result("0,0,1,2,0,L,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,L,1,140")
        expected_result("0,0,1,2,0,P,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,P,1,140")
        expected_result("0,0,1,2,0,C,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,C,1,140")
        expected_result("0,0,1,2,0,C,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,b,1,140")
        expected_result("0,0,1,2,0,C,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,l,1,140")
        expected_result("0,0,1,2,0,C,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,p,1,140")
        expected_result("0,0,1,2,0,C,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0, ,1,140")
        expected_result("0,0,1,2,0,C,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,\t,1,140")
        expected_result("## PARSE ERROR", "0,0,1,2,0, C,1,140")
        expected_result("## PARSE ERROR", "0,0,1,2,0,C ,1,140")
        expected_result("## PARSE ERROR", "0,0,1,2,0,\tC,1,140")
        // Slider: Curve Points
        expected_result("## PARSE ERROR", "0,0,1,2,0,B|,1,140")
        expected_result("0,0,1,2,0,B|0:1,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B|0.9:1.9,1,140")
        expected_result("0,0,1,2,0,B|0:1,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B| 0 : 0 ,1,140")
        expected_result("0,0,1,2,0,B|-2147483648:0,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B|NaN:0,1,140")
        expected_result("0,0,1,2,0,B|-2147483648:0,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B|1e10:0,1,140")
        expected_result("0,0,1,2,0,B|-2147483648:0,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B|-2147483649:0,1,140")
        expected_result("0,0,1,2,0,B|-2147483648:0,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B|2147483584:0,1,140")
        expected_result("0,0,1,2,0,B|2147483520:0,1,140,0|0,0:0|0:0,0:0:0:0:", "0,0,1,2,0,B|2147483583:0,1,140")