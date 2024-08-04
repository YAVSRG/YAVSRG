namespace Prelude.Gameplay.PremadeRulesets

open Prelude
open Prelude.Gameplay

module private DP =

    let windows (judge: int) (ridiculous: bool) =
        let pf = 
            if judge >= 9 then
                0.2f * 45.0f<ms> 
            else 45.0f<ms> / 6.0f * (10.0f - (judge |> float32))

        let ma = pf * 0.5f
        let gr = pf * 2f
        let gd = pf * 3f |> min 135.0f<ms>
        let bd = pf * 4f |> min 180.0f<ms>

        let rd = pf * 0.25f

        if ridiculous then
            [
                -bd, 6
                -gd, 5
                -gr, 4
                -pf, 3
                -ma, 2
                -rd, 1
                rd, 0
                ma, 1
                pf, 2
                gr, 3
                gd, 4
                bd, 5
            ]
        else
            [ -bd, 5; -gd, 4; -gr, 3; -pf, 2; -ma, 1; ma, 0; pf, 1; gr, 2; gd, 3; bd, 4 ]

module Wife3 =

    let create (judge: int) =
        {
            Name =
                if judge = 9 then
                    "Wife3 JUSTICE"
                else
                    sprintf "Wife3 (J%i)" judge
            Description = "Simulates Etterna's scoring system, Wife3"
            Judgements =
                [|
                    {
                        Name = "Marvellous"
                        Color = Color.Aqua
                        BreaksCombo = false
                    }
                    {
                        Name = "Perfect"
                        Color = Color.Yellow
                        BreaksCombo = false
                    }
                    {
                        Name = "Great"
                        Color = Color.FromArgb(0, 255, 100)
                        BreaksCombo = false
                    }
                    {
                        Name = "Good"
                        Color = Color.Blue
                        BreaksCombo = true
                    }
                    {
                        Name = "Bad"
                        Color = Color.Fuchsia
                        BreaksCombo = true
                    }
                    {
                        Name = "Miss"
                        Color = Color.Red
                        BreaksCombo = true
                    }
                |]
            Accuracy =
                {
                    MissWindow = 180.0f<ms>
                    CbrushWindow = 150.0f<ms>
                    Timegates = DP.windows judge false
                    Points = AccuracyPoints.WifeCurve judge
                    HoldNoteBehaviour = HoldNoteBehaviour.JustBreakCombo
                }
            Grading =
                {
                    Grades =
                        [|
                            {
                                Name = "D"
                                Accuracy = 0.0
                                Color = Color.Red
                            }
                            {
                                Name = "C"
                                Accuracy = 0.6
                                Color = Color.Purple
                            }
                            {
                                Name = "B"
                                Accuracy = 0.7
                                Color = Color.Blue
                            }
                            {
                                Name = "A"
                                Accuracy = 0.8
                                Color = Color.Lime
                            }
                            {
                                Name = "A."
                                Accuracy = 0.85
                                Color = Color.Lime
                            }
                            {
                                Name = "A:"
                                Accuracy = 0.9
                                Color = Color.Lime
                            }
                            {
                                Name = "AA"
                                Accuracy = 0.93
                                Color = Color.Gold
                            }
                            {
                                Name = "AA."
                                Accuracy = 0.965
                                Color = Color.Gold
                            }
                            {
                                Name = "AA:"
                                Accuracy = 0.99
                                Color = Color.Gold
                            }
                            {
                                Name = "AAA"
                                Accuracy = 0.997
                                Color = Color.Gold
                            }
                            {
                                Name = "AAA."
                                Accuracy = 0.998
                                Color = Color.Gold
                            }
                            {
                                Name = "AAA:"
                                Accuracy = 0.999
                                Color = Color.Gold
                            }
                            {
                                Name = "AAAA"
                                Accuracy = 0.99955
                                Color = Color.Cyan
                            }
                            {
                                Name = "AAAA."
                                Accuracy = 0.9997
                                Color = Color.Cyan
                            }
                            {
                                Name = "AAAA:"
                                Accuracy = 0.9998
                                Color = Color.Cyan
                            }
                            {
                                Name = "AAAAA"
                                Accuracy = 0.999935
                                Color = Color.White
                            }
                        |]
                    Lamps =
                        [|
                            {
                                Name = "SDCB"
                                Judgement = -1
                                JudgementThreshold = 9
                                Color = Color.FromArgb(255, 160, 160)
                            }
                            {
                                Name = "MF"
                                Judgement = -1
                                JudgementThreshold = 1
                                Color = Color.FromArgb(160, 160, 160)
                            }
                            {
                                Name = "FC"
                                Judgement = -1
                                JudgementThreshold = 0
                                Color = Color.FromArgb(80, 255, 80)
                            }
                            {
                                Name = "SDG"
                                Judgement = 2
                                JudgementThreshold = 9
                                Color = Color.FromArgb(160, 255, 160)
                            }
                            {
                                Name = "BF"
                                Judgement = 2
                                JudgementThreshold = 1
                                Color = Color.FromArgb(200, 160, 255)
                            }
                            {
                                Name = "PFC"
                                Judgement = 2
                                JudgementThreshold = 0
                                Color = Color.FromArgb(255, 255, 80)
                            }
                            {
                                Name = "SDP"
                                Judgement = 1
                                JudgementThreshold = 9
                                Color = Color.FromArgb(255, 255, 160)
                            }
                            {
                                Name = "WF"
                                Judgement = 1
                                JudgementThreshold = 1
                                Color = Color.FromArgb(255, 160, 255)
                            }
                            {
                                Name = "MFC"
                                Judgement = 1
                                JudgementThreshold = 0
                                Color = Color.FromArgb(160, 255, 255)
                            }
                        |]
                }
        }

module ``osu!`` =

    type Mode =
        | Easy
        | NoMod
        | HardRock

    let private nomod_windows (od: float32) =
        (
            16.5f<ms>,
            floor (64f - od * 3f) * 1.0f<ms> + 0.5f<ms>,
            floor (97f - od * 3f) * 1.0f<ms> + 0.5f<ms>,
            floor (127f - od * 3f) * 1.0f<ms> + 0.5f<ms>,
            floor (151f - od * 3f) * 1.0f<ms> + 0.5f<ms>,
            floor (188f - od * 3f) * 1.0f<ms> + 0.5f<ms>
        )
    
    let private hr_windows (od: float32) =
        let (ma, pf, gr, gd, bd, ms) = nomod_windows od
        let hr (window: Time) = floor (5f / 7f<ms> * window) * 1.0f<ms> + 0.5f<ms>
        (
            hr ma,
            hr pf,
            hr gr,
            hr gd,
            hr bd,
            hr ms
        )
    
    let private ez_windows (od: float32) =
        let (ma, pf, gr, gd, bd, ms) = nomod_windows od
        let ez (window: Time) = floor (7f / 5f<ms> * window) * 1.0f<ms> + 0.5f<ms>
        (
            ez ma,
            ez pf,
            ez gr,
            ez gd,
            ez bd,
            ez ms
        )

    let private windows (od: float32) (mode: Mode) =
        match mode with
        | Easy -> ez_windows od
        | NoMod -> nomod_windows od
        | HardRock -> hr_windows od

    let private gates_from_windows ((ma, pf, gr, gd, bd, _): Time * Time * Time * Time * Time * Time) =
        [ -bd, 5; -gd, 4; -gr, 3; -pf, 2; -ma, 1; ma, 0; pf, 1; gr, 2; gd, 3; bd, 4 ]

    let private ln_windows (od: float32) (mode: Mode) =
        let (ma, pf, gr, gd, bd, _) = windows od mode
        {
            Window320 = floor (ma * 1.2f</ms>) * 1.0f<ms> + 0.5f<ms>
            Window300 = floor (pf * 1.1f</ms>) * 1.0f<ms> + 0.5f<ms>
            Window200 = gr
            Window100 = gd
            Window50 = bd
            // todo: idk whats going on with the easy/hr overhold windows but this matches experimental data
            WindowOverhold200 = 
                let base_window = floor (43f - od * 3f) * 1.0f<ms> + 0.5f<ms>
                match mode with
                | Easy -> floor (base_window * 1.4f</ms>) * 1.0f<ms> - 0.5f<ms>
                | HardRock -> floor (base_window / 1.4f<ms>) * 1.0f<ms> + 1.5f<ms>
                | NoMod -> base_window
            WindowOverhold100 =
                let base_window = floor (103f - od * 3f) * 1.0f<ms> + 0.5f<ms>
                match mode with
                | Easy -> floor (base_window * 1.4f</ms>) * 1.0f<ms> - 0.5f<ms>
                | HardRock -> floor (base_window / 1.4f<ms>) * 1.0f<ms> + 1.5f<ms>
                | NoMod -> base_window
        }

    let create (od: float32) (mode: Mode) : Ruleset =
        {
            Name = sprintf "osu! (OD%.1f%s)" od (match mode with NoMod -> "" | Easy -> " +EZ" | HardRock -> " +HR")
            Description = "Simulates osu!'s scoring system"
            Judgements =
                [|
                    {
                        Name = "300g"
                        Color = Color.Aqua
                        BreaksCombo = false
                    }
                    {
                        Name = "300"
                        Color = Color.Yellow
                        BreaksCombo = false
                    }
                    {
                        Name = "200"
                        Color = Color.FromArgb(0, 255, 100)
                        BreaksCombo = false
                    }
                    {
                        Name = "100"
                        Color = Color.FromArgb(0, 160, 255)
                        BreaksCombo = false
                    }
                    {
                        Name = "50"
                        Color = Color.FromArgb(160, 160, 160)
                        BreaksCombo = false
                    }
                    {
                        Name = "MISS"
                        Color = Color.FromArgb(255, 80, 80)
                        BreaksCombo = true
                    }
                |]
            Accuracy =
                {
                    MissWindow = floor (188f - od * 3f) * 1.0f<ms> + 0.5f<ms>
                    CbrushWindow = floor (151f - od * 3f) * 1.0f<ms> + 0.5f<ms>
                    Timegates = windows od mode |> gates_from_windows
                    Points = AccuracyPoints.Weights(300.0, [| 300.0; 300.0; 200.0; 100.0; 50.0; 0.0 |])
                    HoldNoteBehaviour = HoldNoteBehaviour.Osu (ln_windows od mode)
                }
            Grading =
                {
                    Grades =
                        [|
                            {
                                Name = "D"
                                Accuracy = 0.0
                                Color = Color.FromArgb(255, 80, 80)
                            }
                            {
                                Name = "C"
                                Accuracy = 0.7
                                Color = Color.FromArgb(255, 80, 255)
                            }
                            {
                                Name = "B"
                                Accuracy = 0.8
                                Color = Color.FromArgb(0, 80, 255)
                            }
                            {
                                Name = "A"
                                Accuracy = 0.9
                                Color = Color.FromArgb(0, 255, 100)
                            }
                            {
                                Name = "S"
                                Accuracy = 0.95
                                Color = Color.FromArgb(246, 234, 128)
                            }
                            {
                                Name = "SS"
                                Accuracy = 1.0
                                Color = Color.FromArgb(255, 255, 160)
                            }
                        |]
                    Lamps =
                        [|
                            {
                                Name = "SDCB"
                                Judgement = -1
                                JudgementThreshold = 9
                                Color = Color.FromArgb(255, 160, 160)
                            }
                            {
                                Name = "1CB"
                                Judgement = -1
                                JudgementThreshold = 1
                                Color = Color.FromArgb(160, 160, 160)
                            }
                            {
                                Name = "FC"
                                Judgement = -1
                                JudgementThreshold = 0
                                Color = Color.FromArgb(0, 255, 160)
                            }
                            {
                                Name = "PERF"
                                Judgement = 3
                                JudgementThreshold = 0
                                Color = Color.FromArgb(255, 255, 160)
                            }
                            {
                                Name = "SDG"
                                Judgement = 2
                                JudgementThreshold = 9
                                Color = Color.FromArgb(160, 255, 160)
                            }
                            {
                                Name = "1G"
                                Judgement = 2
                                JudgementThreshold = 1
                                Color = Color.FromArgb(200, 160, 255)
                            }
                            {
                                Name = "SS"
                                Judgement = 2
                                JudgementThreshold = 0
                                Color = Color.FromArgb(255, 255, 160)
                            }
                            {
                                Name = "1KK"
                                Judgement = 1
                                JudgementThreshold = 0
                                Color = Color.FromArgb(160, 255, 255)
                            }
                        |]
                }
        }

module SC =

    let private miss_penalty_points (judge: int) =
        match judge with
        | 2 -> -4.0
        | 3 -> -6.0
        | 4 -> -10.0
        | 5 -> -16.0
        | 6 -> -26.0
        | 7 -> -47.0
        | 8 -> -100.0
        | 9 -> -200.0
        | _ -> -10.0

    let create (judge: int) =
        {
            Name = sprintf "SC (J%i)" judge
            Description = "The 'official' scoring system of Interlude, tuned for a balanced experience"
            Judgements =
                [|
                    {
                        Name = "Marvellous"
                        Color = Color.Aqua
                        BreaksCombo = false
                    }
                    {
                        Name = "Perfect"
                        Color = Color.Yellow
                        BreaksCombo = false
                    }
                    {
                        Name = "Great"
                        Color = Color.FromArgb(0, 255, 100)
                        BreaksCombo = false
                    }
                    {
                        Name = "Good"
                        Color = Color.Blue
                        BreaksCombo = true
                    }
                    {
                        Name = "Bad"
                        Color = Color.Fuchsia
                        BreaksCombo = true
                    }
                    {
                        Name = "Miss"
                        Color = Color.Red
                        BreaksCombo = true
                    }
                |]
            Accuracy =
                {
                    MissWindow = 180.0f<ms>
                    CbrushWindow = 90.0f<ms>
                    Timegates = DP.windows judge false
                    Points = 
                        let miss_penalty = miss_penalty_points judge
                        AccuracyPoints.Weights(10.0, [| 10.0; 9.0; 5.0; -5.0; miss_penalty; miss_penalty |])
                    HoldNoteBehaviour =
                        HoldNoteBehaviour.Normal
                            {|
                                JudgementIfDropped = 3
                                JudgementIfOverheld = 3
                            |}
                }
            Grading =
                {
                    Grades =
                        [|
                            {
                                Name = "D"
                                Accuracy = 0.0
                                Color = Color.FromArgb(200, 163, 155)
                            }
                            {
                                Name = "C-"
                                Accuracy = 0.89995
                                Color = Color.FromArgb(194, 162, 182)
                            }
                            {
                                Name = "C"
                                Accuracy = 0.90995
                                Color = Color.FromArgb(202, 153, 183)
                            }
                            {
                                Name = "B-"
                                Accuracy = 0.91995
                                Color = Color.FromArgb(163, 190, 207)
                            }
                            {
                                Name = "B"
                                Accuracy = 0.92995
                                Color = Color.FromArgb(149, 193, 220)
                            }
                            {
                                Name = "A-"
                                Accuracy = 0.93995
                                Color = Color.FromArgb(148, 210, 180)
                            }
                            {
                                Name = "A"
                                Accuracy = 0.94995
                                Color = Color.FromArgb(134, 227, 183)
                            }
                            {
                                Name = "A+"
                                Accuracy = 0.95995
                                Color = Color.FromArgb(127, 231, 139)
                            }
                            {
                                Name = "S-"
                                Accuracy = 0.96995
                                Color = Color.FromArgb(237, 205, 140)
                            }
                            {
                                Name = "S"
                                Accuracy = 0.97995
                                Color = Color.FromArgb(246, 234, 128)
                            }
                            {
                                Name = "S+"
                                Accuracy = 0.98995
                                Color = Color.FromArgb(235, 200, 220)
                            }
                        |]
                    Lamps =
                        [|
                            {
                                Name = "SDCB"
                                Judgement = -1
                                JudgementThreshold = 9
                                Color = Color.FromArgb(255, 160, 160)
                            }
                            {
                                Name = "1CB"
                                Judgement = -1
                                JudgementThreshold = 1
                                Color = Color.FromArgb(160, 160, 160)
                            }
                            {
                                Name = "FC"
                                Judgement = -1
                                JudgementThreshold = 0
                                Color = Color.FromArgb(80, 255, 80)
                            }
                            {
                                Name = "SDG"
                                Judgement = 2
                                JudgementThreshold = 9
                                Color = Color.FromArgb(160, 255, 160)
                            }
                            {
                                Name = "1G"
                                Judgement = 2
                                JudgementThreshold = 1
                                Color = Color.FromArgb(200, 160, 255)
                            }
                            {
                                Name = "PFC"
                                Judgement = 2
                                JudgementThreshold = 0
                                Color = Color.FromArgb(255, 255, 80)
                            }
                            {
                                Name = "SDP"
                                Judgement = 1
                                JudgementThreshold = 9
                                Color = Color.FromArgb(255, 255, 160)
                            }
                            {
                                Name = "1P"
                                Judgement = 1
                                JudgementThreshold = 1
                                Color = Color.FromArgb(255, 160, 255)
                            }
                            {
                                Name = "MFC"
                                Judgement = 1
                                JudgementThreshold = 0
                                Color = Color.FromArgb(160, 255, 255)
                            }
                        |]
                }
        }

module Ex_Score =

    type Type =
        {
            Name: string
            Critical: Time
            Near: Time
            MissWindow: Time
        }

    let mizu: Type =
        {
            Name = "Mizu"
            Critical = 45.0f<ms>
            Near = 150.0f<ms>
            MissWindow = 150.0f<ms>
        }

    let create (data: Type) =
        {
            Name = sprintf "EXSCORE (%s)" data.Name
            Description = "EXSCORE-based score system designed by qqp"
            Judgements =
                [|
                    {
                        Name = "CRITICAL"
                        Color = Color.Aqua
                        BreaksCombo = false
                    }
                    {
                        Name = "NEAR"
                        Color = Color.Yellow
                        BreaksCombo = false
                    }
                    {
                        Name = "BREAK"
                        Color = Color.FromArgb(0, 255, 100)
                        BreaksCombo = true
                    }
                    {
                        Name = "MISS"
                        Color = Color.FromArgb(255, 100, 100)
                        BreaksCombo = true
                    }
                |]
            Accuracy =
                {
                    MissWindow = 150.0f<ms>
                    CbrushWindow = 150.0f<ms>
                    Timegates = [ -data.Critical, 1; data.Critical, 0; data.Near, 1 ]
                    Points = AccuracyPoints.Weights(1.0, [| 1.0; 0.5; 0.0; 0.0 |])
                    HoldNoteBehaviour =
                        HoldNoteBehaviour.Normal
                            {|
                                JudgementIfDropped = 2
                                JudgementIfOverheld = 1
                            |}
                }
            Grading =
                {
                    Grades =
                        [|
                            {
                                Name = "B"
                                Accuracy = 0.90
                                Color = Color.FromArgb(202, 153, 183)
                            }
                            {
                                Name = "B+"
                                Accuracy = 0.92
                                Color = Color.FromArgb(163, 190, 207)
                            }
                            {
                                Name = "A"
                                Accuracy = 0.93
                                Color = Color.FromArgb(149, 193, 220)
                            }
                            {
                                Name = "A+"
                                Accuracy = 0.94
                                Color = Color.FromArgb(148, 210, 180)
                            }
                            {
                                Name = "AA"
                                Accuracy = 0.95
                                Color = Color.FromArgb(134, 227, 183)
                            }
                            {
                                Name = "AA+"
                                Accuracy = 0.96
                                Color = Color.FromArgb(127, 231, 139)
                            }
                            {
                                Name = "AAA"
                                Accuracy = 0.97
                                Color = Color.FromArgb(237, 205, 140)
                            }
                            {
                                Name = "AAA+"
                                Accuracy = 0.98
                                Color = Color.FromArgb(246, 234, 128)
                            }
                            {
                                Name = "S"
                                Accuracy = 0.99
                                Color = Color.FromArgb(235, 200, 220)
                            }
                            {
                                Name = "S+"
                                Accuracy = 0.995
                                Color = Color.FromArgb(209, 156, 255)
                            }
                        |]
                    Lamps =
                        [|
                            {
                                Name = "SDCB"
                                Judgement = -1
                                JudgementThreshold = 9
                                Color = Color.FromArgb(255, 160, 160)
                            }
                            {
                                Name = "1BREAK"
                                Judgement = -1
                                JudgementThreshold = 1
                                Color = Color.FromArgb(180, 180, 180)
                            }
                            {
                                Name = "FC"
                                Judgement = -1
                                JudgementThreshold = 0
                                Color = Color.FromArgb(255, 255, 80)
                            }
                            {
                                Name = "SDN"
                                Judgement = 1
                                JudgementThreshold = 9
                                Color = Color.FromArgb(255, 255, 160)
                            }
                            {
                                Name = "1N"
                                Judgement = 1
                                JudgementThreshold = 1
                                Color = Color.FromArgb(255, 255, 200)
                            }
                            {
                                Name = "PFC"
                                Judgement = 1
                                JudgementThreshold = 0
                                Color = Color.FromArgb(160, 255, 255)
                            }
                        |]
                }
        }

module Noodles =

    let RULESET =
        {
            Name = "Noodles"
            Description = "Release timing practice ruleset"
            Judgements =
                [|
                    {
                        Name = "CRITICAL"
                        Color = Color.Aqua
                        BreaksCombo = false
                    }
                    {
                        Name = "NEAR"
                        Color = Color.Yellow
                        BreaksCombo = false
                    }
                    {
                        Name = "POOR"
                        Color = Color.FromArgb(0, 255, 100)
                        BreaksCombo = false
                    }
                    {
                        Name = "BREAK"
                        Color = Color.Fuchsia
                        BreaksCombo = true
                    }
                    {
                        Name = "MISS"
                        Color = Color.FromArgb(255, 100, 100)
                        BreaksCombo = true
                    }
                |]
            Accuracy =
                {
                    MissWindow = 150.0f<ms>
                    CbrushWindow = 150.0f<ms>
                    Timegates = [ -135f<ms>, 3; -90f<ms>, 2; -45f<ms>, 1; 45f<ms>, 0; 90f<ms>, 1; 135f<ms>, 2; 150f<ms>, 3 ]
                    Points = AccuracyPoints.Weights(1.0, [| 1.0; 0.9; 0.5; 0.0; 0.0 |])
                    HoldNoteBehaviour = HoldNoteBehaviour.OnlyJudgeReleases 2
                }
            Grading =
                {
                    Grades =
                        [|
                            {
                                Name = "C"
                                Accuracy = 0.90
                                Color = Color.FromArgb(202, 153, 183)
                            }
                            {
                                Name = "C+"
                                Accuracy = 0.92
                                Color = Color.FromArgb(163, 190, 207)
                            }
                            {
                                Name = "B"
                                Accuracy = 0.93
                                Color = Color.FromArgb(149, 193, 220)
                            }
                            {
                                Name = "B+"
                                Accuracy = 0.94
                                Color = Color.FromArgb(148, 210, 180)
                            }
                            {
                                Name = "A"
                                Accuracy = 0.95
                                Color = Color.FromArgb(134, 227, 183)
                            }
                            {
                                Name = "A+"
                                Accuracy = 0.96
                                Color = Color.FromArgb(127, 231, 139)
                            }
                            {
                                Name = "AA"
                                Accuracy = 0.97
                                Color = Color.FromArgb(237, 205, 140)
                            }
                            {
                                Name = "AAA"
                                Accuracy = 0.98
                                Color = Color.FromArgb(246, 234, 128)
                            }
                            {
                                Name = "S"
                                Accuracy = 0.99
                                Color = Color.FromArgb(235, 200, 220)
                            }
                        |]
                    Lamps =
                        [|
                            {
                                Name = "FC"
                                Judgement = -1
                                JudgementThreshold = 0
                                Color = Color.FromArgb(255, 255, 80)
                            }
                            {
                                Name = "PFC"
                                Judgement = 1
                                JudgementThreshold = 0
                                Color = Color.FromArgb(160, 255, 255)
                            }
                        |]
                }
        }
