namespace Prelude.Gameplay.Rulesets

open Prelude
open Prelude.Gameplay.Rulesets

module Wife3 =

    let create (judge: int) : Ruleset =

        if judge > 9 || judge < 4 then failwithf "Judge must be between 4 and 9, '%i' is not supported" judge

        let PERFECT_WINDOW = 
            if judge = 9 then 9.0f<ms / rate>
            else
                let scale = (10.0f - float32 judge) / 6.0f
                45.0f<ms / rate> * scale

        {
            Name = if judge = 9 then "Wife3 JUSTICE" else sprintf "Wife3 J%i" judge
            Description = "Simulates Etterna's scoring system, Wife3"
            Judgements =
                [|
                    {
                        Name = "Marvelous"
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
                        TimingWindows = Some(-180.0f<ms / rate>, 180.0f<ms / rate>)
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
                        Color = Color.Lime
                    }
                    {
                        Name = "AA."
                        Accuracy = 0.965
                        Color = Color.Lime
                    }
                    {
                        Name = "AA:"
                        Accuracy = 0.99
                        Color = Color.Lime
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
                        Requirement = LampRequirement.ComboBreaksAtMost 9
                        Color = Color.FromArgb(255, 160, 160)
                    }
                    {
                        Name = "MF"
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
                        Name = "BF"
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
                        Name = "WF"
                        Requirement = LampRequirement.JudgementAtMost (1, 1)
                        Color = Color.FromArgb(255, 160, 255)
                    }
                    {
                        Name = "MFC"
                        Requirement = LampRequirement.JudgementAtMost (1, 0)
                        Color = Color.FromArgb(160, 255, 255)
                    }
                |]
            Accuracy = AccuracyPoints.WifeCurve judge
            HitMechanics = 
                { 
                    NotePriority = NotePriority.Etterna
                    GhostTapJudgement = None
                }
            HoldMechanics = HoldMechanics.OnlyRequireHold 180.0f<ms / rate>
            Formatting = { DecimalPlaces = DecimalPlaces.TWO }
        }