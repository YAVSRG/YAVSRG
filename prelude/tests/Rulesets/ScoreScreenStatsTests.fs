namespace Prelude.Tests.Rulesets

open NUnit.Framework
open Percyqaz.Common
open Prelude
open Prelude.Formats
open Prelude.Formats.Osu
open Prelude.Data.OsuClientInterop
open Prelude.Gameplay.Rulesets
open Prelude.Gameplay.Scoring
open Prelude.Tests.Helpers

module ScoreScreenStatsTests =

    let TEST_SCORE =
        let beatmap_path = "./Data/Cardboard Box - He He He (DannyPX) [SPEEEDDD!!!].osu"
        let beatmap = Beatmap.FromFile beatmap_path |> expect

        let chart =
            (Osu_To_Interlude.convert
                beatmap
                {
                    Config = ConversionOptions.Pack("osu!", None, LinkAssetFiles)
                    Source = beatmap_path
                }
             |> expect)
                .Chart

        let osu_replay =
            OsuReplay.TryReadFile "./Data/Lylcaruis - Cardboard Box - He He He [SPEEEDDD!!!] (2023-09-29) OsuMania.osr"
            |> Option.get

        let replay = OsuReplay.decode (osu_replay, chart.FirstNote, 1.0f<rate>)
        let ruleset = SC.create 4
        ScoreProcessor.ProcessEntireReplay(ruleset, replay, chart, 1.0f<rate>)

    [<Test>]
    let BasicEndToEnd () =

        let result = ScoreScreenStats.calculate TEST_SCORE [| true; true; true; true |]
        printfn "%A" result

        Assert.Pass()

    [<Test>]
    let TwoNotes_CorrectOutput () =

        let note_data = NotesBuilder(4).Note(0.0f<ms>).Note(1000.0f<ms>).Build()

        let replay =
            ReplayBuilder()
                .KeyDownFor(-30.0f<ms>, 30.0f<ms>)
                .KeyDownFor(500.0f<ms>, 30.0f<ms>)
                .KeyDownFor(990.0f<ms>, 30.0f<ms>)
                .Build()

        let score =
            ScoreProcessor.ProcessEntireReplay(SC.create 4, replay, note_data, 1.0f<rate>)

        let result = ScoreScreenStats.calculate score [| true; true; true; true |]
        printfn "%A" result

        Assert.AreEqual((2, 2), result.Notes)
        Assert.AreEqual((0, 0), result.Releases)
        Assert.AreEqual(1, result.GhostTaps)
        Assert.AreEqual((-30.0f<ms / rate>, 0.0f<ms / rate>), result.TapRange)
        Assert.AreEqual((0.0f<ms / rate>, 0.0f<ms / rate>), result.ReleaseRange)
        Assert.AreEqual(-20.0f, round (result.TapMean |> float32))
        Assert.AreEqual(0.0f, round (result.ReleaseMean |> float32))

    [<Test>]
    let OneNote_CorrectOutput () =

        let note_data = NotesBuilder(4).Note(0.0f<ms>).Build()

        let replay = ReplayBuilder().KeyDownFor(-30.0f<ms>, 30.0f<ms>).Build()

        let score = ScoreProcessor.ProcessEntireReplay(SC.create 4, replay, note_data, 1.0f<rate>)

        let result = ScoreScreenStats.calculate score [| true; true; true; true |]
        printfn "%A" result

        Assert.AreEqual((1, 1), result.Notes)
        Assert.AreEqual((0, 0), result.Releases)
        Assert.AreEqual(0, result.GhostTaps)
        Assert.AreEqual((-30.0f<ms / rate>, 0.0f<ms / rate>), result.TapRange)
        Assert.AreEqual((0.0f<ms / rate>, 0.0f<ms / rate>), result.ReleaseRange)
        Assert.AreEqual(-30.0f, round (result.TapMean |> float32))
        Assert.AreEqual(0.0f, round (result.ReleaseMean |> float32))

    [<Test>]
    let ColumnFilter_TwoNotes () =

        let note_data =
            NotesBuilder(4)
                .Note(0.0f<ms>)
                .Note(1000.0f<ms>)
                .Note(2000.0f<ms>, 2)
                .Note(3000.0f<ms>, 2)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownFor(-30.0f<ms>, 30.0f<ms>)
                .KeyDownFor(500.0f<ms>, 30.0f<ms>)
                .KeyDownFor(990.0f<ms>, 30.0f<ms>)
                .KeyDownFor(1980.0f<ms>, 30.0f<ms>, 2)
                .Build()

        let score = ScoreProcessor.ProcessEntireReplay(SC.create 4, replay, note_data, 1.0f<rate>)

        let result = ScoreScreenStats.calculate score [| false; false; true; true |]
        printfn "%A" result

        Assert.AreEqual((1, 2), result.Notes)
        Assert.AreEqual((0, 0), result.Releases)
        Assert.AreEqual(0, result.GhostTaps)
        Assert.AreEqual((-20.0f<ms / rate>, 0.0f<ms / rate>), result.TapRange)
        Assert.AreEqual((0.0f<ms / rate>, 0.0f<ms / rate>), result.ReleaseRange)
        Assert.AreEqual(-20.0f, round (result.TapMean |> float32))
        Assert.AreEqual(0.0f, round (result.ReleaseMean |> float32))

    [<Test>]
    let ColumnFilter_ZeroNotes () =

        let note_data =
            NotesBuilder(4)
                .Note(0.0f<ms>)
                .Note(1000.0f<ms>)
                .Note(2000.0f<ms>, 2)
                .Note(3000.0f<ms>, 2)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownFor(-30.0f<ms>, 30.0f<ms>)
                .KeyDownFor(500.0f<ms>, 30.0f<ms>)
                .KeyDownFor(990.0f<ms>, 30.0f<ms>)
                .KeyDownFor(1980.0f<ms>, 30.0f<ms>, 2)
                .Build()

        let score = ScoreProcessor.ProcessEntireReplay(SC.create 4, replay, note_data, 1.0f<rate>)

        let result = ScoreScreenStats.calculate score [| false; false; false; true |]
        printfn "%A" result

        Assert.AreEqual((0, 0), result.Notes)
        Assert.AreEqual((0, 0), result.Releases)
        Assert.AreEqual(0, result.GhostTaps)
        Assert.AreEqual((0.0f<ms / rate>, 0.0f<ms / rate>), result.TapRange)
        Assert.AreEqual((0.0f<ms / rate>, 0.0f<ms / rate>), result.ReleaseRange)
        Assert.AreEqual(0.0f, round (result.TapMean |> float32))
        Assert.AreEqual(0.0f, round (result.ReleaseMean |> float32))