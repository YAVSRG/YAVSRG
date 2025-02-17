namespace Prelude.Tests.Charts

open System
open NUnit.Framework
open Prelude
open Prelude.Charts
open Prelude.ChartFormats.osu
open Prelude.ChartFormats
open Prelude.Data.Library

module Conversions =

    [<Test>]
    let OsuMania_Notes_RoundTrip () =

        for seed, keys in [ 123, 4; 0, 7; 123, 10 ] do

            let chart = ChartFuzzBuilder.generate (keys, seed)

            let osu_notes = OsuExport.notes_to_hitobjects chart.Notes chart.Keys
            let interlude_notes = Osu_To_Interlude.convert_hit_objects osu_notes chart.Keys

            Assert.AreEqual(chart.Notes, interlude_notes)

    [<Test>]
    let OsuMania_TimingPoints_RoundTrip () =

        let simplified_sv (sv: TimeArray<float32>) : TimeArray<float32> =
            seq {
                let mutable previous_value = 1.0f

                for s in sv do
                    if abs (s.Data - previous_value) > 0.005f then
                        yield
                            {
                                Time = s.Time
                                Data = MathF.Round(s.Data, 3)
                            }

                        previous_value <- s.Data
            }
            |> Array.ofSeq

        for seed, keys in [ 123, 4; 0, 7; 123, 10 ] do

            let chart = ChartFuzzBuilder.generate (keys, seed)

            let most_common_mspb = Chart.find_most_common_bpm chart
            let osu_points = OsuExport.convert_timing_points chart.BPM chart.SV most_common_mspb

            let interlude_bpm, interlude_sv =
                Osu_To_Interlude.convert_timing_points osu_points chart.LastNote

            Assert.AreEqual(chart.BPM, interlude_bpm)
            Assert.AreEqual(simplified_sv chart.SV, simplified_sv interlude_sv)

    [<Test>]
    let OsuMania_TimingPoints_SingleBPM () =

        let one_bpm =
            [|
                {
                    Time = 10f<ms>
                    Data =
                        {
                            MsPerBeat = 500.0f<ms / beat>
                            Meter = 4<beat>
                        }
                }
            |]

        let osu_points = OsuExport.convert_timing_points one_bpm [||] 500.0f<ms / beat>

        let interlude_bpm, interlude_sv =
            Osu_To_Interlude.convert_timing_points osu_points 1000.0f<ms>

        printfn "%A" osu_points

        Assert.AreEqual(one_bpm, interlude_bpm)
        Assert.AreEqual([||], interlude_sv)

    [<Test>]
    let OsuMania_TimingPoints_VariableBPM () =

        let many_bpms =
            [|
                {
                    Time = 10f<ms>
                    Data =
                        {
                            MsPerBeat = 500.0f<ms / beat>
                            Meter = 4<beat>
                        }
                }
                {
                    Time = 40f<ms>
                    Data =
                        {
                            MsPerBeat = 250.0f<ms / beat>
                            Meter = 4<beat>
                        }
                }
                {
                    Time = 50f<ms>
                    Data =
                        {
                            MsPerBeat = 500.0f<ms / beat>
                            Meter = 4<beat>
                        }
                }
                {
                    Time = 100f<ms>
                    Data =
                        {
                            MsPerBeat = 1000.0f<ms / beat>
                            Meter = 4<beat>
                        }
                }
                {
                    Time = 110f<ms>
                    Data =
                        {
                            MsPerBeat = 500.0f<ms / beat>
                            Meter = 4<beat>
                        }
                }
            |]

        let osu_points = OsuExport.convert_timing_points many_bpms [||] 500.0f<ms / beat>

        let interlude_bpm, interlude_sv =
            Osu_To_Interlude.convert_timing_points osu_points 1000.0f<ms>

        printfn "%A" osu_points

        Assert.AreEqual(many_bpms, interlude_bpm)
        Assert.AreEqual([||], interlude_sv)

    [<Test>]
    let OsuToInterlude_ForgiveStackedNotes_CaseA () =
        let osu_notes =
            [
                HitObject.CreateManiaNote(4, 0, 1000.0f<ms>)
                HitObject.CreateManiaNote(4, 0, 1000.0f<ms>)
                HitObject.CreateManiaNote(4, 0, 2000.0f<ms>)
            ]

        try
            let interlude_notes = Osu_To_Interlude.convert_hit_objects osu_notes 4
            Chart.pretty_print interlude_notes
            Assert.AreEqual(2, interlude_notes.Length)
        with ConversionSkipException reason ->
            Assert.Fail(reason)

    [<Test>]
    let OsuToInterlude_ForgiveStackedNotes_CaseB () =
        let osu_notes =
            [
                HitObject.CreateManiaNote(4, 0, 1000.0f<ms>)
                HitObject.CreateManiaHold(4, 0, 1000.0f<ms>, 2000.0f<ms>)
                HitObject.CreateManiaNote(4, 0, 3000.0f<ms>)
            ]

        try
            let interlude_notes = Osu_To_Interlude.convert_hit_objects osu_notes 4
            Chart.pretty_print interlude_notes
            Assert.AreEqual(3, interlude_notes.Length)
        with ConversionSkipException reason ->
            Assert.Fail(reason)

    [<Test>]
    let OsuToInterlude_ForgiveStackedNotes_CaseC () =
        let osu_notes =
            [
                HitObject.CreateManiaHold(4, 0, 1000.0f<ms>, 2000.0f<ms>)
                HitObject.CreateManiaNote(4, 0, 1000.0f<ms>)
                HitObject.CreateManiaNote(4, 0, 3000.0f<ms>)
            ]

        try
            let interlude_notes = Osu_To_Interlude.convert_hit_objects osu_notes 4
            Chart.pretty_print interlude_notes
            Assert.AreEqual(3, interlude_notes.Length)
        with ConversionSkipException reason ->
            Assert.Fail(reason)

    [<Test>]
    let OsuToInterlude_UnacceptableStackedNote_CaseA () =
        let osu_notes =
            [
                HitObject.CreateManiaHold(4, 0, 1000.0f<ms>, 2000.0f<ms>)
                HitObject.CreateManiaNote(4, 0, 2000.0f<ms>)
            ]

        try
            let interlude_notes = Osu_To_Interlude.convert_hit_objects osu_notes 4
            Assert.Fail()
        with ConversionSkipException reason ->
            Assert.Pass(reason)

    [<Test>]
    let OsuToInterlude_UnacceptableStackedNote_CaseB () =
        let osu_notes =
            [
                HitObject.CreateManiaHold(4, 0, 1000.0f<ms>, 2000.0f<ms>)
                HitObject.CreateManiaNote(4, 0, 1500.0f<ms>)
            ]

        try
            let interlude_notes = Osu_To_Interlude.convert_hit_objects osu_notes 4
            Assert.Fail()
        with ConversionSkipException reason ->
            Assert.Pass(reason)

    [<Test>]
    let OsuToInterlude_UnacceptableStackedNote_CaseC () =
        let osu_notes =
            [
                HitObject.CreateManiaHold(4, 0, 1000.0f<ms>, 2000.0f<ms>)
                HitObject.CreateManiaHold(4, 0, 1500.0f<ms>, 2500.0f<ms>)
            ]

        try
            let interlude_notes = Osu_To_Interlude.convert_hit_objects osu_notes 4
            Assert.Fail()
        with ConversionSkipException reason ->
            Assert.Pass(reason)

    [<Test>]
    let OsuToInterlude_UnacceptableStackedNote_CaseD () =
        let osu_notes =
            [
                HitObject.CreateManiaHold(4, 0, 1000.0f<ms>, 2000.0f<ms>)
                HitObject.CreateManiaHold(4, 0, 2000.0f<ms>, 3000.0f<ms>)
            ]

        try
            let interlude_notes = Osu_To_Interlude.convert_hit_objects osu_notes 4
            Assert.Fail()
        with ConversionSkipException reason ->
            Assert.Pass(reason)