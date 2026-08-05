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
        Assert.AreEqual(1000.0f<ms / beat>, result_bpm.[1].Data.MsPerBeat)

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

    [<Test>]
    let OsuConverter_InheritedPoints_MinAndMaxMult () =
        let points =
            [
                "0,250,4,1,0,10,1,0"
                "1000,-5000,4,1,0,10,0,0"
                "2000,-10000,4,1,0,10,0,0"
                "3000,-20000,4,1,0,10,0,0"
                "4000,-50,4,1,0,10,0,0"
                "5000,-10,4,1,0,10,0,0"
                "6000,-1,4,1,0,10,0,0"
            ]
            |> List.map TimingPoint.FromString

        let bpm, sv = OsuConverter.convert_timing_points points 20000.0f<ms>

        Assert.AreEqual(1, bpm.Length)
        Assert.AreEqual({ Time = 1000.0f<ms>; Data = 0.02f }, sv.[0])
        Assert.AreEqual({ Time = 2000.0f<ms>; Data = 0.01f }, sv.[1])
        // at 3000: SV would be 0.005x but 0.01x is the minimum, overall no effect so pruned out
        Assert.AreEqual({ Time = 4000.0f<ms>; Data = 2.0f }, sv.[2])
        Assert.AreEqual({ Time = 5000.0f<ms>; Data = 10.0f }, sv.[3])
        // at 6000: SV would be 100x but 10x is the maximum, overall no effect so pruned out
        Assert.AreEqual(4, sv.Length)

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

        Assert.AreEqual("4860B4B8254E5C89D293361D168203103F91BE9F2B2294F9693BB82705565BCC", converted.Chart.Hash())

        let chart_meta =
            ChartMeta.CreateFromImport(Timestamp.now(), converted, K AssetLocation.Missing)

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

        Assert.AreEqual("4860B4B8254E5C89D293361D168203103F91BE9F2B2294F9693BB82705565BCC", round_trip.Chart.Hash())
