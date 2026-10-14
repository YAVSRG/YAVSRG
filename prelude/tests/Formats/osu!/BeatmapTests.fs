namespace Prelude.Tests.Formats

open NUnit.Framework
open Prelude.Formats.Osu

module BeatmapTests =

    [<Test>]
    let Beatmap_Defaults () =
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

        let beatmap = Beatmap.FromLines(data)
        Assert.IsEmpty(beatmap.Objects)
        Assert.IsEmpty(beatmap.Events)
        Assert.IsEmpty(beatmap.Timing)

    [<Test>]
    let Beatmap_Headers_Split () =
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
                "[HitObjects]"
                "256,192,0,1,0,0:0:0:0:"
                "[Metadata]"
                "Tags:tags here"
                "[HitObjects]"
                "256,192,1000,1,0,0:0:0:0:"
            |]

        let beatmap = Beatmap.FromLines(data)
        // Data from both sections used together
        Assert.AreEqual([ "tags"; "here" ], beatmap.Metadata.Tags)
        Assert.AreEqual("Test scenario", beatmap.Metadata.Version)

        // Both hitobjects are loaded as one list
        Assert.AreEqual(2, beatmap.Objects.Length)

    [<Test>]
    let Beatmap_Later_Value_Priority () =
        let data =
            [|
                "osu beatmap format v14"
                "[General]"
                "AudioFilename: audio.mp3"
                "[Metadata]"
                "Title:This value is ignored"
                "Title:Test scenario"
                "Artist:Prelude"
                "Creator:Percyqaz"
                "Version:Test scenario"
            |]

        let beatmap = Beatmap.FromLines(data)
        Assert.AreEqual("Test scenario", beatmap.Metadata.Title)

    [<Test>]
    let Beatmap_Comments_And_Ignored_Events () =
        let data =
            [|
                "osu beatmap format v14"
                " "
                "[General]"
                "AudioFilename: audio.mp3"
                ""
                "[Metadata]"
                "Title:This value is ignored"
                "Title:Test scenario"
                "Artist:Prelude"
                "Creator:Percyqaz"
                "Version:Test scenario"
                "    "
                "[Events]"
                "//Background and Video events"
                "0,0,example.jpg,0,0"
                "//Break Periods"
                "2,127352,131524"
                "//Storyboard Layer 3 (Foreground)"
                "Sprite,Foreground,Centre,sprite.png,320,240"
                " F,0,330,1786,0,1"
                " F,0,16355,17812,1,0"
            |]

        let beatmap = Beatmap.FromLines(data)
        Assert.AreEqual("Test scenario", beatmap.Metadata.Title)
        Assert.AreEqual(2, beatmap.Events.Length)
        Assert.AreEqual([ Background("example.jpg", 0, 0); Break(127352, 131524) ], beatmap.Events)
