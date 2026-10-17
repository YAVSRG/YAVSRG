namespace Prelude.Tests.Formats.Osu

open NUnit.Framework
open Prelude.Formats.Osu

module HitSampleTests =

    // In general, do what osu!stable would do if it parsed this input, and then wrote it back to an .osu file
    // to verify, I put test cases into an .osu file, hit CTRL-L in the editor, then save and see what gets written

    // ## PARSE ERROR = Error on load, Line removed from .osu on save, or File is rendered unplayable
    // OR I have decided to do something different for sanity, if so this is commented

    let parse_hs (expected: string, input: string) =
        let result =
            try
                HitSample.FromString(input).ToString()
            with _ ->
                "## PARSE ERROR"

        Assert.AreEqual(expected, result)

    [<Test>]
    let HitSample_ValidParses () =

        parse_hs("## PARSE ERROR", "")
        parse_hs("## PARSE ERROR", " ")
        parse_hs("0:0:0:0:", " 0 :  0  :   0   :    0    :")
        parse_hs("0:0:0:0:", "00:00:00:00:")
        parse_hs("-1:5:-2147483648:100: File", "-1 : 5 : -2147483648 : 101 : File")

        parse_hs("## PARSE ERROR", "0:0:0:-2147483649:")
        parse_hs("## PARSE ERROR", "0:0:0:2147483648:")
        parse_hs("0:0:0:0:", "0:0:0:-2147483648:")
        parse_hs("0:0:0:100:", "0:0:0:2147483647:")
