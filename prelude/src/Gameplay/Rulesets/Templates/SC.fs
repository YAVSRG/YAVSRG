namespace Prelude.Gameplay.Rulesets

open Prelude
open Prelude.Gameplay

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