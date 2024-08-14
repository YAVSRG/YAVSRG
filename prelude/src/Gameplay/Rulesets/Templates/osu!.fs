namespace Prelude.Gameplay.Rulesets

open Prelude
open Prelude.Gameplay

module ``osu!`` =

    let ln_judgement
        (windows: OsuLnWindows)
        (head_delta: Time)
        (tail_delta: Time)
        (overhold: bool)
        (dropped: bool)
        : JudgementId =
        let tail_delta_abs = Time.abs tail_delta
        let head_delta_abs = Time.abs head_delta

        if not dropped && not overhold then

            let mean = (tail_delta_abs + head_delta_abs) * 0.5f

            if tail_delta < -windows.Window50 then 5

            elif head_delta_abs < windows.Window320 && mean < windows.Window320 then 0 // 300g
            elif head_delta_abs < windows.Window300 && mean < windows.Window300 then 1 // 300
            elif head_delta_abs < windows.Window200 && mean < windows.Window200 then 2 // 200
            elif head_delta_abs < windows.Window100 && mean < windows.Window100 then 3 // 100
            elif head_delta_abs < windows.Window50 && mean < windows.Window50 then 4 // 50
            else 5 // miss

        elif dropped then

            if tail_delta_abs < windows.Window50 then 4 else 5

        else

            if head_delta_abs < windows.WindowOverhold200 then 2 // 200
            elif head_delta_abs < windows.WindowOverhold100 then 3 // 100
            else 4 // 50

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