namespace Prelude.Tests.Charts

open NUnit.Framework
open Percyqaz.Common
open Prelude
open Prelude.Charts
open Prelude.Formats.Osu
open Prelude.Formats.Conversions
open Prelude.Data.Library
open Prelude.Tests.Helpers

module OsuConverterTests =

    [<Test>]
    let OsuConverter_StackedTimingPoints () =
        let points =
            [
                TimingPoint.CreateBPM(0.0f<ms>, 500.0f<ms / beat>, 4)
                TimingPoint.CreateSV(0.0f<ms>, 0.5f)
                TimingPoint.CreateBPM(100.0f<ms>, 250.0f<ms / beat>, 4)
                TimingPoint.CreateBPM(100.0f<ms>, 1000.0f<ms / beat>, 4)
                TimingPoint.CreateSV(100f<ms>, 2.0f)
            ]

        let result_bpm, result_sv = OsuConverter.convert_timing_points points 2000.0f<ms>

        Assert.AreEqual(1, result_sv.Length)

        Assert.AreEqual(100.0f<ms>, result_sv.[0].Time)
        Assert.AreEqual(2.0f, result_sv.[0].Data)

        Assert.AreEqual(2, result_bpm.Length)

    [<Test>]
    let OsuConverter_ForgiveStackedNotes_CaseA () =
        let osu_notes =
            [
                HitObject.CreateManiaNote(4, 0, 1000.0f<ms>)
                HitObject.CreateManiaNote(4, 0, 1000.0f<ms>)
                HitObject.CreateManiaNote(4, 0, 2000.0f<ms>)
            ]

        try
            let interlude_notes = OsuConverter.convert_hit_objects osu_notes 4
            Chart.pretty_print interlude_notes
            Assert.AreEqual(2, interlude_notes.Length)
        with ConversionSkipException reason ->
            Assert.Fail(reason)

    [<Test>]
    let OsuConverter_ForgiveStackedNotes_CaseB () =
        let osu_notes =
            [
                HitObject.CreateManiaNote(4, 0, 1000.0f<ms>)
                HitObject.CreateManiaHold(4, 0, 1000.0f<ms>, 2000.0f<ms>)
                HitObject.CreateManiaNote(4, 0, 3000.0f<ms>)
            ]

        try
            let interlude_notes = OsuConverter.convert_hit_objects osu_notes 4
            Chart.pretty_print interlude_notes
            Assert.AreEqual(3, interlude_notes.Length)
        with ConversionSkipException reason ->
            Assert.Fail(reason)

    [<Test>]
    let OsuConverter_ForgiveStackedNotes_CaseC () =
        let osu_notes =
            [
                HitObject.CreateManiaHold(4, 0, 1000.0f<ms>, 2000.0f<ms>)
                HitObject.CreateManiaNote(4, 0, 1000.0f<ms>)
                HitObject.CreateManiaNote(4, 0, 3000.0f<ms>)
            ]

        try
            let interlude_notes = OsuConverter.convert_hit_objects osu_notes 4
            Chart.pretty_print interlude_notes
            Assert.AreEqual(3, interlude_notes.Length)
        with ConversionSkipException reason ->
            Assert.Fail(reason)

    [<Test>]
    let OsuConverter_UnacceptableStackedNote_CaseA () =
        let osu_notes =
            [
                HitObject.CreateManiaHold(4, 0, 1000.0f<ms>, 2000.0f<ms>)
                HitObject.CreateManiaNote(4, 0, 2000.0f<ms>)
            ]

        try
            let _ = OsuConverter.convert_hit_objects osu_notes 4
            Assert.Fail()
        with ConversionSkipException reason ->
            Assert.Pass(reason)

    [<Test>]
    let OsuConverter_UnacceptableStackedNote_CaseB () =
        let osu_notes =
            [
                HitObject.CreateManiaHold(4, 0, 1000.0f<ms>, 2000.0f<ms>)
                HitObject.CreateManiaNote(4, 0, 1500.0f<ms>)
            ]

        try
            let _ = OsuConverter.convert_hit_objects osu_notes 4
            Assert.Fail()
        with ConversionSkipException reason ->
            Assert.Pass(reason)

    [<Test>]
    let OsuConverter_UnacceptableStackedNote_CaseC () =
        let osu_notes =
            [
                HitObject.CreateManiaHold(4, 0, 1000.0f<ms>, 2000.0f<ms>)
                HitObject.CreateManiaHold(4, 0, 1500.0f<ms>, 2500.0f<ms>)
            ]

        try
            let _ = OsuConverter.convert_hit_objects osu_notes 4
            Assert.Fail()
        with ConversionSkipException reason ->
            Assert.Pass(reason)

    [<Test>]
    let OsuConverter_UnacceptableStackedNote_CaseD () =
        let osu_notes =
            [
                HitObject.CreateManiaHold(4, 0, 1000.0f<ms>, 2000.0f<ms>)
                HitObject.CreateManiaHold(4, 0, 2000.0f<ms>, 3000.0f<ms>)
            ]

        try
            let _ = OsuConverter.convert_hit_objects osu_notes 4
            Assert.Fail()
        with ConversionSkipException reason ->
            Assert.Pass(reason)

    [<Test>]
    let OsuConverter_TimingPointsCanHaveNonIntegerTimes () =
        let points =
            [
                "200,250,4,1,0,10,1,0"
                "5000,250,4,1,0,10,1,0"
                "5000.1,25,4,1,0,10,1,0"
                "5000.2,250,4,1,0,10,1,0"
            ]
            |> List.map TimingPoint.FromString

        let bpm, sv = OsuConverter.convert_timing_points points 200000.0f<ms>

        Assert.AreEqual(
            [|
                { Time = 200.0f<ms>; Data = { MsPerBeat = 250.0f<ms / beat>; Meter = 4<beat> } }
                { Time = 5000.0f<ms>; Data = { MsPerBeat = 250.0f<ms / beat>; Meter = 4<beat> } }
                { Time = 5000.1f<ms>; Data = { MsPerBeat = 25.0f<ms / beat>; Meter = 4<beat> } }
                { Time = 5000.2f<ms>; Data = { MsPerBeat = 250.0f<ms / beat>; Meter = 4<beat> } }
            |],
            bpm
        )

        Assert.AreEqual([| { Time = 5000.1f<ms>; Data = 10.0f }; { Time = 5000.2f<ms>; Data = 1.0f } |], sv)

    [<Test>]
    let OsuConverter_NegativeBpm_TreatedAsPositive () =
        let points =
            [ "0,250,4,1,0,10,1,0"; "15000,-500,4,1,0,10,1,0" ] |> List.map TimingPoint.FromString

        let bpm, sv = OsuConverter.convert_timing_points points 20000.0f<ms>

        Assert.AreEqual(1, sv.Length)
        Assert.AreEqual({ Time = 15000.0f<ms>; Data = 0.5f }, sv.[0])

        Assert.AreEqual(2, bpm.Length)
        Assert.AreEqual(0.0f<ms>, bpm.[0].Time)
        Assert.AreEqual(250.0f<ms / beat>, bpm.[0].Data.MsPerBeat)
        Assert.AreEqual(15000.0f<ms>, bpm.[1].Time)
        Assert.AreEqual(500.0f<ms / beat>, bpm.[1].Data.MsPerBeat)

    // todo: test to cover svs clamped to 0.1x - 10x (that will currently fail)
    // todo: test to cover SV pruning logic

    [<Test>]
    let RoundTrip_Fuzzed_Notes () =
        for seed, keys in [ 123, 4; 0, 7; 123, 10 ] do
            let chart = ChartFuzzer.Generate(keys, seed)
            let osu_notes = OsuExport.notes_to_hitobjects chart.Notes chart.Keys
            let interlude_notes = OsuConverter.convert_hit_objects osu_notes chart.Keys

            Assert.AreEqual(chart.Notes, interlude_notes)

    [<Test>]
    let RoundTrip_Fuzzed_TimingPoints () =
        let inline round_sv (sv: TimeArray<float32>) =
            cleaned_sv(sv) |> TimeArray.map(fun x -> System.MathF.Round(x, 3))

        for seed, keys in [ 123, 4; 0, 7; 123, 10 ] do
            let chart = ChartFuzzer.Generate(keys, seed)
            let most_common_mspb = Chart.find_most_common_bpm chart
            let osu_points = OsuExport.convert_timing_points chart.BPM chart.SV most_common_mspb

            let interlude_bpm, interlude_sv =
                OsuConverter.convert_timing_points osu_points chart.LastNote

            Assert.AreEqual(chart.BPM, interlude_bpm)
            Assert.AreEqual(round_sv chart.SV, round_sv interlude_sv)

    [<Test>]
    let RoundTrip_TimingPoints_SingleBPM () =
        let one_bpm =
            [|
                { Time = 10f<ms>; Data = { MsPerBeat = 500.0f<ms / beat>; Meter = 4<beat> } }
            |]

        let osu_points = OsuExport.convert_timing_points one_bpm [||] 500.0f<ms / beat>

        let interlude_bpm, interlude_sv =
            OsuConverter.convert_timing_points osu_points 1000.0f<ms>

        Assert.AreEqual(one_bpm, interlude_bpm)
        Assert.AreEqual([||], interlude_sv)

    [<Test>]
    let RoundTrip_TimingPoints_VariableBPM () =
        let many_bpms =
            [|
                { Time = 10f<ms>; Data = { MsPerBeat = 500.0f<ms / beat>; Meter = 4<beat> } }
                { Time = 40f<ms>; Data = { MsPerBeat = 250.0f<ms / beat>; Meter = 4<beat> } }
                { Time = 50f<ms>; Data = { MsPerBeat = 500.0f<ms / beat>; Meter = 4<beat> } }
                { Time = 100f<ms>; Data = { MsPerBeat = 1000.0f<ms / beat>; Meter = 4<beat> } }
                { Time = 110f<ms>; Data = { MsPerBeat = 500.0f<ms / beat>; Meter = 4<beat> } }
            |]

        let osu_points = OsuExport.convert_timing_points many_bpms [||] 500.0f<ms / beat>

        let interlude_bpm, interlude_sv =
            OsuConverter.convert_timing_points osu_points 1000.0f<ms>

        Assert.AreEqual(many_bpms, interlude_bpm)
        Assert.AreEqual([||], interlude_sv)

    [<Test>]
    let RoundTrip_BackbeatManiac () =
        let beatmap =
            Beatmap.TryReadFromFile("./Data/Camellia - Backbeat Maniac (Evening) [Rewind VIP].osu") |> expect

        let converted =
            OsuConverter.convert
                beatmap
                {
                    Options = ConversionOptions.Pack("osu!", None, Link)
                    OriginalPath = "./Data/Camellia - Backbeat Maniac (Evening) [Rewind VIP].osu"
                }
            |> expect

        match converted.Chart.CheckForErrors() with
        | Ok _ -> ()
        | Error reason -> Assert.Fail(reason)

        Assert.AreEqual(converted.Chart.Hash(), "785CAC8E416B32EF134D16D504E1F71E43AC595219EC347B89E896758F5F2795")

        let chart_meta =
            ChartMeta.CreateFromImport (Timestamp.now()) (K AssetLocation.Missing) converted

        let exported_beatmap =
            OsuExport.convert { HP = 8; OD = 8 } converted.Chart chart_meta

        let round_trip =
            OsuConverter.convert
                exported_beatmap
                {
                    Options = ConversionOptions.Pack("osu!", None, Link)
                    OriginalPath = "./Data/Camellia - Backbeat Maniac (Evening) [Rewind VIP].osu"
                }
            |> expect

        Assert.AreEqual(round_trip.Chart.Hash(), "785CAC8E416B32EF134D16D504E1F71E43AC595219EC347B89E896758F5F2795")
