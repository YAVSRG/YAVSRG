namespace Prelude.Tests.Charts

open NUnit.Framework
open Prelude
open Prelude.Charts
open Prelude.Tests.Helpers

module ChartTests =

    [<Test>]
    let Fuzz_CreatesValidCharts() =

        for seed, keys in [ 123, 4; 0, 7; 123, 10 ] do
            let chart = ChartFuzzer.Generate(keys, seed)
            match Chart.check chart with
            | Ok _ -> ()
            | Error reason -> Assert.Fail(reason)

        Assert.Pass()

    [<Test>]
    let Fuzz_PrettyPrint() =

        let chart = ChartFuzzer.Generate(5, 0)

        Chart.pretty_print chart.Notes

        Assert.Pass()

    [<Test>]
    let Fuzz_Deterministic_Hashing_Deterministic() =

        let test_expected_hash(keys: int, seed: int, expected_hash: string) : unit =
            let chart = ChartFuzzer.Generate(keys, seed)
            Assert.AreEqual(expected_hash, Chart.hash chart)

        test_expected_hash(4, 123, "66FCABE08A7008B62872F80D2AD2DD144FCCF0B693E0907CD7F5F984B2A04B21")
        test_expected_hash(7, 0, "A03A034E4B836D919906467F60CF07503CF503583638DB13FC871D026E2600D6")
        test_expected_hash(10, 123, "73FF85CB6B2B8673EA6FD7DF317F290E1B2C7A1B81677E2E3F4D7353EF246ABD")

        Assert.Pass()

    [<Test>]
    let Scale() =

        let chart = ChartFuzzer.Generate(4, 123)
        let twice_as_long = Chart.scale 2.0f</rate> chart
        let scaled_to_original = Chart.scale 0.5f</rate> twice_as_long

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