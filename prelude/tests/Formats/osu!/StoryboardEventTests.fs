namespace Prelude.Tests.Formats

open NUnit.Framework
open Prelude.Formats.Osu

module StoryboardEventTests =
    
    // Prelude can only parse Background/Video/Sample events, events used in full storyboards are skipped on parse
    // Prelude has a representation for more complicated storyboard events (but not loops) and can write them to file
    // (I used to use it as a storyboard generator for a few projects)

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
