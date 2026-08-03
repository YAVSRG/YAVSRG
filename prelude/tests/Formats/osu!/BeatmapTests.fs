namespace Prelude.Tests.Formats

open NUnit.Framework
open Prelude.Formats.Osu

module BeatmapTests =

    [<Test>]
    let Header_Objects_Example() =
        let data =
            [|
                "osu beatmap format v14"
                "[General]"
                "AudioFilename: audio.mp3"
                "[Metadata]"
                "Title:Test scenario"
                "Artist:Prelude"
                "Creator:Percyqaz"
                "Version:Test scenario"
            |]
            
        Beatmap.FromLines(data)
        |> printfn "%A"