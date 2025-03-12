namespace Prelude.Gameplay.Rulesets

open Prelude

module SC =

    let create (judge: int) : Ruleset =

        let PERFECT_WINDOW =
            if judge = 9 then 9.0f<ms / rate>
            else
                let scale = (10.0f - float32 judge) / 6.0f
                45.0f<ms / rate> * scale

        let MISS_PENALTY_POINTS =
            match judge with
            | 2 -> -0.4
            | 3 -> -0.6
            | 4 -> -1.0
            | 5 -> -1.6
            | 6 -> -2.6
            | 7 -> -4.7
            | 8 -> -10.0
            | 9 -> -20.0
            | _ -> failwithf "Judge must be between 2 and 9, '%i' is not supported" judge

        {
            Name = sprintf "SC J%i" judge
            Description = "The 'official' scoring system of Interlude, tuned for a balanced experience"
            Judgements =
                [|
                    {
                        Name = "Marvellous"
                        Color = Color.Aqua
                        BreaksCombo = false
                        TimingWindows = Some(-PERFECT_WINDOW * 0.5f, PERFECT_WINDOW * 0.5f)
                    }
                    {
                        Name = "Perfect"
                        Color = Color.Yellow
                        BreaksCombo = false
                        TimingWindows = Some(-PERFECT_WINDOW, PERFECT_WINDOW)
                    }
                    {
                        Name = "Great"
                        Color = Color.FromArgb(0, 255, 100)
                        BreaksCombo = false
                        TimingWindows = Some(-PERFECT_WINDOW * 2.0f, PERFECT_WINDOW * 2.0f)
                    }
                    {
                        Name = "Good"
                        Color = Color.Blue
                        BreaksCombo = true
                        TimingWindows = Some(-PERFECT_WINDOW * 3.0f, PERFECT_WINDOW * 3.0f)
                    }
                    {
                        Name = "Bad"
                        Color = Color.Fuchsia
                        BreaksCombo = true
                        TimingWindows = Some(-PERFECT_WINDOW * 4.0f |> min -180.0f<ms / rate>, PERFECT_WINDOW * 4.0f |> max 180.0f<ms / rate>)
                    }
                    {
                        Name = "Miss"
                        Color = Color.Red
                        BreaksCombo = true
                        TimingWindows = None
                    }
                |]
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
                        Requirement = LampRequirement.ComboBreaksAtMost 9
                        Color = Color.FromArgb(255, 160, 160)
                    }
                    {
                        Name = "1CB"
                        Requirement = LampRequirement.ComboBreaksAtMost 1
                        Color = Color.FromArgb(160, 160, 160)
                    }
                    {
                        Name = "FC"
                        Requirement = LampRequirement.ComboBreaksAtMost 0
                        Color = Color.FromArgb(80, 255, 80)
                    }
                    {
                        Name = "SDG"
                        Requirement = LampRequirement.JudgementAtMost (2, 9)
                        Color = Color.FromArgb(160, 255, 160)
                    }
                    {
                        Name = "1G"
                        Requirement = LampRequirement.JudgementAtMost (2, 1)
                        Color = Color.FromArgb(200, 160, 255)
                    }
                    {
                        Name = "PFC"
                        Requirement = LampRequirement.JudgementAtMost (2, 0)
                        Color = Color.FromArgb(255, 255, 80)
                    }
                    {
                        Name = "SDP"
                        Requirement = LampRequirement.JudgementAtMost (1, 9)
                        Color = Color.FromArgb(255, 255, 160)
                    }
                    {
                        Name = "1P"
                        Requirement = LampRequirement.JudgementAtMost (1, 1)
                        Color = Color.FromArgb(255, 160, 255)
                    }
                    {
                        Name = "MFC"
                        Requirement = LampRequirement.JudgementAtMost (1, 0)
                        Color = Color.FromArgb(160, 255, 255)
                    }
                |]
            Accuracy = AccuracyPoints.PointsPerJudgement [| 1.0; 0.9; 0.5; -0.5; MISS_PENALTY_POINTS; MISS_PENALTY_POINTS |]
            HitMechanics =
                {
                    NotePriority = NotePriority.Interlude 90.0f<ms / rate>
                    GhostTapJudgement = None
                }
            HoldMechanics = HoldMechanics.CombineHeadAndTail (HeadTailCombineRule.HeadJudgementOr (-180.0f<ms / rate>, 180.0f<ms / rate>, 3, 3))
            Formatting = { DecimalPlaces = DecimalPlaces.TWO }
        }