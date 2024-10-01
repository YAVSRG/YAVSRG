namespace Prelude.Gameplay.Rulesets.OLD

open System
open Prelude

module Wife3 =

    // lifted from https://github.com/etternagame/etterna/blob/0a7bd768cffd6f39a3d84d76964097e43011ce33/Themes/_fallback/Scripts/10%20Scores.lua#L606-L627
    let wife_curve (judge: int) (delta: Time) =
        let erf =
            // was this really necessary
            let a1 = 0.254829592
            let a2 = -0.284496736
            let a3 = 1.421413741
            let a4 = -1.453152027
            let a5 = 1.061405429
            let p = 0.3275911

            fun (x: float) ->
                let sign = if x < 0.0 then -1.0 else 1.0
                let x = Math.Abs x
                let t = 1.0 / (1.0 + p * x)

                let y =
                    1.0 - (((((a5 * t + a4) * t) + a3) * t + a2) * t + a1) * t * Math.Exp(-x * x)

                sign * y

        let delta = float delta |> Math.Abs

        let scale = (10.0 - float judge) / 6.0
        let miss_weight = -2.75
        let ridic = 5.0 * scale
        let boo_window = 180.0 * scale
        let ts_pow = 0.75
        let zero = 65.0 * Math.Pow(scale, ts_pow)
        let dev = 22.7 * Math.Pow(scale, ts_pow)

        if delta <= ridic then
            1.0
        elif delta <= zero then
            erf ((zero - delta) / dev)
        elif delta <= boo_window then
            (delta - zero) * miss_weight / (boo_window - zero)
        else
            miss_weight

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
                        Name = "Marvelous"
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
