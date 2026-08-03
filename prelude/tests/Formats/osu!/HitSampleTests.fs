namespace Prelude.Tests.Formats

open NUnit.Framework
open Prelude.Formats.Osu

module HitSampleTests =

    // In general, do what osu!stable would do if it parsed this input, and then wrote it back to an .osu file
    // to verify, I put test cases into an .osu file, hit CTRL-L in the editor, then save and see what gets written

    // ## PARSE ERROR = Error on load, Line removed from .osu on save, or File is rendered unplayable
    // OR I have decided to do something different for sanity, if so this is commented

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
