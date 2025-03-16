namespace Prelude.Tests.Charts

open NUnit.Framework
open Prelude
open Prelude.Charts

module Charts =

    [<Test>]
    let Fuzz_CreatesValidCharts() =

        for seed, keys in [ 123, 4; 0, 7; 123, 10 ] do
            let chart = ChartFuzzBuilder.generate (keys, seed)
            match Chart.check chart with
            | Ok _ -> ()
            | Error reason -> Assert.Fail(reason)

        Assert.Pass()

    [<Test>]
    let Fuzz_PrettyPrint() =

        let chart = ChartFuzzBuilder.generate (5, 0)

        Chart.pretty_print chart.Notes

        Assert.Pass()

    [<Test>]
    let Fuzz_Deterministic_Hashing_Deterministic() =

        let TEST_DATA =
            [
                123, 4, "7F7ED43C7820701F541548FCA9A6692B0B6109A9F0D78FDFBE84D2BEA3E0A4F0"
                0, 7, "9478FFC413A3FCFB65055CAF69A6F25E9B7EF5AAA73532DF8A99D320EEAD4689"
                123, 10, "61F0F1DDE1B53E6A8382AB0879B2FF86B2CA7E0B25D29E7357BD3DCEA078EB99"
            ]

        for seed, keys, expected_hash in TEST_DATA do
            let chart = ChartFuzzBuilder.generate (keys, seed)

            Assert.AreEqual(expected_hash, Chart.hash chart)

        Assert.Pass()

    [<Test>]
    let Scale() =

        let chart = ChartFuzzBuilder.generate (4, 123)
        let twice_as_long = Chart.scale 2.0f<rate> chart
        let scaled_to_original = Chart.scale 0.5f<rate> twice_as_long

        Assert.AreNotEqual(Chart.hash twice_as_long, Chart.hash chart)
        Assert.AreEqual(Chart.hash scaled_to_original, Chart.hash chart)

    [<Test>]
    let Check_NoNotes() =

        let chart =
            {
                Keys = 4
                Notes = [||]
                BPM = [| { Time = 100.0f<ms>; Data = { MsPerBeat = 50.0f<ms / beat>; Meter = 4<beat> } } |]
                SV = [||]
            }

        match Chart.check chart with
        | Ok _ -> Assert.Fail()
        | Error reason -> Assert.Pass(reason)

    [<Test>]
    let Check_NoBPMs() =

        let chart =
            {
                Keys = 4
                Notes = [| { Time = 0.0f<ms>; Data = [| NoteType.NORMAL; NoteType.NOTHING; NoteType.NOTHING; NoteType.NOTHING |] } |]
                BPM = [||]
                SV = [||]
            }

        match Chart.check chart with
        | Ok _ -> Assert.Fail()
        | Error reason -> Assert.Pass(reason)

    [<Test>]
    let Check_EmptyNoteRows() =

        let empty_row =
            {
                Keys = 4
                Notes =
                    [|
                        {
                            Time = 0.0f<ms>
                            Data = [| NoteType.NORMAL; NoteType.NOTHING; NoteType.NOTHING; NoteType.NOTHING |]
                        }
                        {
                            Time = 100.0f<ms>
                            Data = [| NoteType.NOTHING; NoteType.NOTHING; NoteType.NOTHING; NoteType.NOTHING |]
                        }
                    |]
                BPM = [| { Time = 100.0f<ms>; Data = { MsPerBeat = 50.0f<ms / beat>; Meter = 4<beat> } } |]
                SV = [||]
            }

        match Chart.check empty_row with
        | Ok _ -> Assert.Fail()
        | Error reason -> printfn "%s" reason

        let only_hold_middles =
            {
                Keys = 4
                Notes =
                    [|
                        {
                            Time = 0.0f<ms>
                            Data = [| NoteType.HOLDHEAD; NoteType.NOTHING; NoteType.NOTHING; NoteType.NOTHING |]
                        }
                        {
                            Time = 100.0f<ms>
                            Data = [| NoteType.HOLDBODY; NoteType.NOTHING; NoteType.NOTHING; NoteType.NOTHING |]
                        }
                        {
                            Time = 200.0f<ms>
                            Data = [| NoteType.HOLDTAIL; NoteType.NOTHING; NoteType.NOTHING; NoteType.NOTHING |]
                        }
                    |]
                BPM = [| { Time = 100.0f<ms>; Data = { MsPerBeat = 50.0f<ms / beat>; Meter = 4<beat> } } |]
                SV = [||]
            }

        match Chart.check only_hold_middles with
        | Ok _ -> Assert.Fail()
        | Error reason -> printfn "%s" reason

        Assert.Pass()

    [<Test>]
    let Check_LongNotes() =

        let correct =
            {
                Keys = 4
                Notes =
                    [|
                        {
                            Time = 0.0f<ms>
                            Data = [| NoteType.HOLDHEAD; NoteType.NOTHING; NoteType.NOTHING; NoteType.NOTHING |]
                        }
                        {
                            Time = 100.0f<ms>
                            Data = [| NoteType.HOLDTAIL; NoteType.NOTHING; NoteType.NOTHING; NoteType.NOTHING |]
                        }
                    |]
                BPM = [| { Time = 100.0f<ms>; Data = { MsPerBeat = 50.0f<ms / beat>; Meter = 4<beat> } } |]
                SV = [||]
            }

        match Chart.check correct with
        | Ok _ -> ()
        | Error reason -> Assert.Fail(reason)

        let missing_middle =
            {
                Keys = 4
                Notes =
                    [|
                        {
                            Time = 0.0f<ms>
                            Data = [| NoteType.HOLDHEAD; NoteType.NOTHING; NoteType.NOTHING; NoteType.NOTHING |]
                        }
                        {
                            Time = 100.0f<ms>
                            Data = [| NoteType.NOTHING; NoteType.NORMAL; NoteType.NOTHING; NoteType.NOTHING |]
                        }
                        {
                            Time = 200.0f<ms>
                            Data = [| NoteType.HOLDTAIL; NoteType.NOTHING; NoteType.NOTHING; NoteType.NOTHING |]
                        }
                    |]
                BPM = [| { Time = 100.0f<ms>; Data = { MsPerBeat = 50.0f<ms / beat>; Meter = 4<beat> } } |]
                SV = [||]
            }

        match Chart.check missing_middle with
        | Ok _ -> Assert.Fail()
        | Error reason -> printfn "%s" reason

        let missing_tail =
            {
                Keys = 4
                Notes =
                    [|
                        {
                            Time = 0.0f<ms>
                            Data = [| NoteType.HOLDHEAD; NoteType.NOTHING; NoteType.NOTHING; NoteType.NOTHING |]
                        }
                        {
                            Time = 100.0f<ms>
                            Data = [| NoteType.HOLDBODY; NoteType.NORMAL; NoteType.NOTHING; NoteType.NOTHING |]
                        }
                        {
                            Time = 200.0f<ms>
                            Data = [| NoteType.NOTHING; NoteType.NORMAL; NoteType.NOTHING; NoteType.NOTHING |]
                        }
                    |]
                BPM = [| { Time = 100.0f<ms>; Data = { MsPerBeat = 50.0f<ms / beat>; Meter = 4<beat> } } |]
                SV = [||]
            }

        match Chart.check missing_tail with
        | Ok _ -> Assert.Fail()
        | Error reason -> printfn "%s" reason

        let unexpected_middle =
            {
                Keys = 4
                Notes =
                    [|
                        {
                            Time = 0.0f<ms>
                            Data = [| NoteType.HOLDBODY; NoteType.NOTHING; NoteType.NOTHING; NoteType.NOTHING |]
                        }
                        {
                            Time = 100.0f<ms>
                            Data = [| NoteType.NOTHING; NoteType.NORMAL; NoteType.NOTHING; NoteType.NOTHING |]
                        }
                    |]
                BPM = [| { Time = 100.0f<ms>; Data = { MsPerBeat = 50.0f<ms / beat>; Meter = 4<beat> } } |]
                SV = [||]
            }

        match Chart.check unexpected_middle with
        | Ok _ -> Assert.Fail()
        | Error reason -> printfn "%s" reason

        let unexpected_head =
            {
                Keys = 4
                Notes =
                    [|
                        {
                            Time = 0.0f<ms>
                            Data = [| NoteType.HOLDHEAD; NoteType.NOTHING; NoteType.NOTHING; NoteType.NOTHING |]
                        }
                        {
                            Time = 100.0f<ms>
                            Data = [| NoteType.HOLDHEAD; NoteType.NORMAL; NoteType.NOTHING; NoteType.NOTHING |]
                        }
                        {
                            Time = 200.0f<ms>
                            Data = [| NoteType.HOLDTAIL; NoteType.NORMAL; NoteType.NOTHING; NoteType.NOTHING |]
                        }
                    |]
                BPM = [| { Time = 100.0f<ms>; Data = { MsPerBeat = 50.0f<ms / beat>; Meter = 4<beat> } } |]
                SV = [||]
            }

        match Chart.check unexpected_head with
        | Ok _ -> Assert.Fail()
        | Error reason -> printfn "%s" reason

        Assert.Pass()

    [<Test>]
    let Check_NoteTimestamps() =

        let stacked =
            {
                Keys = 4
                Notes =
                    [|
                        {
                            Time = 100.0f<ms>
                            Data = [| NoteType.NORMAL; NoteType.NOTHING; NoteType.NOTHING; NoteType.NOTHING |]
                        }
                        {
                            Time = 100.0f<ms>
                            Data = [| NoteType.NORMAL; NoteType.NOTHING; NoteType.NOTHING; NoteType.NOTHING |]
                        }
                    |]
                BPM = [| { Time = 100.0f<ms>; Data = { MsPerBeat = 50.0f<ms / beat>; Meter = 4<beat> } } |]
                SV = [||]
            }

        match Chart.check stacked with
        | Ok _ -> Assert.Fail()
        | Error reason -> printfn "%s" reason

        let backwards_notes =
            {
                Keys = 4
                Notes =
                    [|
                        {
                            Time = 100.0f<ms>
                            Data = [| NoteType.NORMAL; NoteType.NOTHING; NoteType.NOTHING; NoteType.NOTHING |]
                        }
                        {
                            Time = 0.0f<ms>
                            Data = [| NoteType.NORMAL; NoteType.NOTHING; NoteType.NOTHING; NoteType.NOTHING |]
                        }
                    |]
                BPM = [| { Time = 100.0f<ms>; Data = { MsPerBeat = 50.0f<ms / beat>; Meter = 4<beat> } } |]
                SV = [||]
            }

        match Chart.check backwards_notes with
        | Ok _ -> Assert.Fail()
        | Error reason -> printfn "%s" reason

        let inf =
            {
                Keys = 4
                Notes =
                    [|
                        {
                            Time = Time.infinity
                            Data = [| NoteType.NORMAL; NoteType.NOTHING; NoteType.NOTHING; NoteType.NOTHING |]
                        }
                        {
                            Time = 0.0f<ms>
                            Data = [| NoteType.NORMAL; NoteType.NOTHING; NoteType.NOTHING; NoteType.NOTHING |]
                        }
                    |]
                BPM = [| { Time = 100.0f<ms>; Data = { MsPerBeat = 50.0f<ms / beat>; Meter = 4<beat> } } |]
                SV = [||]
            }

        match Chart.check inf with
        | Ok _ -> Assert.Fail()
        | Error reason -> printfn "%s" reason

        let nan =
            {
                Keys = 4
                Notes =
                    [|
                        {
                            Time = 0.0f<ms> / 0.0f
                            Data = [| NoteType.NORMAL; NoteType.NOTHING; NoteType.NOTHING; NoteType.NOTHING |]
                        }
                        {
                            Time = 0.0f<ms>
                            Data = [| NoteType.NORMAL; NoteType.NOTHING; NoteType.NOTHING; NoteType.NOTHING |]
                        }
                    |]
                BPM = [| { Time = 100.0f<ms>; Data = { MsPerBeat = 50.0f<ms / beat>; Meter = 4<beat> } } |]
                SV = [||]
            }

        match Chart.check nan with
        | Ok _ -> Assert.Fail()
        | Error reason -> printfn "%s" reason

        Assert.Pass()

    [<Test>]
    let Check_BPMTimestamps() =

        let backwards_bpms =
            {
                Keys = 4
                Notes =
                    [|
                        {
                            Time = 0.0f<ms>
                            Data = [| NoteType.NORMAL; NoteType.NOTHING; NoteType.NOTHING; NoteType.NOTHING |]
                        }
                    |]
                BPM =
                    [|
                        {
                            Time = 100.0f<ms>
                            Data = { MsPerBeat = 50.0f<ms / beat>; Meter = 4<beat> }
                        }
                        {
                            Time = 0.0f<ms>
                            Data = { MsPerBeat = 50.0f<ms / beat>; Meter = 4<beat> }
                        }
                    |]
                SV = [||]
            }

        match Chart.check backwards_bpms with
        | Ok _ -> Assert.Fail()
        | Error reason -> printfn "%s" reason

        let inf =
            {
                Keys = 4
                Notes =
                    [|
                        {
                            Time = 0.0f<ms>
                            Data = [| NoteType.NORMAL; NoteType.NOTHING; NoteType.NOTHING; NoteType.NOTHING |]
                        }
                    |]
                BPM =
                    [|
                        {
                            Time = 100.0f<ms>
                            Data = { MsPerBeat = 50.0f<ms / beat>; Meter = 4<beat> }
                        }
                        {
                            Time = Time.infinity
                            Data = { MsPerBeat = 50.0f<ms / beat>; Meter = 4<beat> }
                        }
                    |]
                SV = [||]
            }

        match Chart.check inf with
        | Ok _ -> Assert.Fail()
        | Error reason -> printfn "%s" reason

        let nan =
            {
                Keys = 4
                Notes =
                    [|
                        {
                            Time = 0.0f<ms>
                            Data = [| NoteType.NORMAL; NoteType.NOTHING; NoteType.NOTHING; NoteType.NOTHING |]
                        }
                    |]
                BPM =
                    [|
                        {
                            Time = 0.0f<ms>
                            Data = { MsPerBeat = 50.0f<ms / beat>; Meter = 4<beat> }
                        }
                        {
                            Time = 0.0f<ms> / 0.0f
                            Data = { MsPerBeat = 50.0f<ms / beat>; Meter = 4<beat> }
                        }
                    |]
                SV = [||]
            }

        match Chart.check nan with
        | Ok _ -> Assert.Fail()
        | Error reason -> printfn "%s" reason

        Assert.Pass()

    [<Test>]
    let Check_InvalidBPMs() =

        let negative_bpm =
            {
                Keys = 4
                Notes =
                    [|
                        {
                            Time = 0.0f<ms>
                            Data = [| NoteType.NORMAL; NoteType.NOTHING; NoteType.NOTHING; NoteType.NOTHING |]
                        }
                    |]
                BPM =
                    [|
                        {
                            Time = 0.0f<ms>
                            Data = { MsPerBeat = -50.0f<ms / beat>; Meter = 4<beat> }
                        }
                    |]
                SV = [||]
            }

        match Chart.check negative_bpm with
        | Ok _ -> Assert.Fail()
        | Error reason -> printfn "%s" reason

        let zero_meter =
            {
                Keys = 4
                Notes =
                    [|
                        {
                            Time = 0.0f<ms>
                            Data = [| NoteType.NORMAL; NoteType.NOTHING; NoteType.NOTHING; NoteType.NOTHING |]
                        }
                    |]
                BPM =
                    [|
                        {
                            Time = 0.0f<ms>
                            Data = { MsPerBeat = 50.0f<ms / beat>; Meter = 0<beat> }
                        }
                    |]
                SV = [||]
            }

        match Chart.check zero_meter with
        | Ok _ -> Assert.Fail()
        | Error reason -> printfn "%s" reason

        let nan_bpm =
            {
                Keys = 4
                Notes =
                    [|
                        {
                            Time = 0.0f<ms>
                            Data = [| NoteType.NORMAL; NoteType.NOTHING; NoteType.NOTHING; NoteType.NOTHING |]
                        }
                    |]
                BPM =
                    [|
                        {
                            Time = 0.0f<ms>
                            Data = { MsPerBeat = nanf * 1.0f<ms / beat>; Meter = 4<beat> }
                        }
                    |]
                SV = [||]
            }

        match Chart.check nan_bpm with
        | Ok _ -> Assert.Fail()
        | Error reason -> printfn "%s" reason