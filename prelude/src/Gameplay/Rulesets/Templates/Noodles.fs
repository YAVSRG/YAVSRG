namespace Prelude.Gameplay.Rulesets

open Prelude
open Prelude.Gameplay

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
