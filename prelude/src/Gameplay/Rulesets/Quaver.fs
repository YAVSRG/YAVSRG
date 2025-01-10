namespace Prelude.Gameplay.Rulesets

open Percyqaz.Common
open Prelude
open Prelude.Gameplay.Rulesets

module Quaver =

    // Partial info found https://wiki.quavergame.com/docs/gameplay
    // The rest through experimentation

    type Judgement =
        | Peaceful
        | Lenient
        | Chill
        | Standard
        | Strict
        | Tough
        | Extreme
        | Impossible
        override this.ToString() =
            match this with
            | Standard -> "Standard*"
            | _ -> sprintf "%A" this
        member this.Windows =
            match this with
            | Peaceful -> ( 23.0f<ms / rate>, 57.0f<ms / rate>, 101.0f<ms / rate>, 141.0f<ms / rate>, 169.0f<ms / rate>, 218.0f<ms / rate> )
            | Lenient -> ( 21.0f<ms / rate>, 52.0f<ms / rate>, 91.0f<ms / rate>, 128.0f<ms / rate>, 153.0f<ms / rate>, 198.0f<ms / rate> )
            | Chill -> ( 19.0f<ms / rate>, 47.0f<ms / rate>, 83.0f<ms / rate>, 116.0f<ms / rate>, 139.0f<ms / rate>, 180.0f<ms / rate> )
            | Standard -> ( 18.0f<ms / rate>, 43.0f<ms / rate>, 76.0f<ms / rate>, 106.0f<ms / rate>, 127.0f<ms / rate>, 164.0f<ms / rate> )
            | Strict -> ( 16.0f<ms / rate>, 39.0f<ms / rate>, 69.0f<ms / rate>, 96.0f<ms / rate>, 127.0f<ms / rate>, 164.0f<ms / rate> )
            | Tough -> ( 14.0f<ms / rate>, 35.0f<ms / rate>, 62.0f<ms / rate>, 87.0f<ms / rate>, 127.0f<ms / rate>, 164.0f<ms / rate> )
            | Extreme -> ( 13.0f<ms / rate>, 32.0f<ms / rate>, 57.0f<ms / rate>, 79.0f<ms / rate>, 127.0f<ms / rate>, 164.0f<ms / rate> )
            | Impossible -> ( 8.0f<ms / rate>, 20.0f<ms / rate>, 35.0f<ms / rate>, 49.0f<ms / rate>, 127.0f<ms / rate>, 164.0f<ms / rate> )

        static member LIST = [|Peaceful; Lenient; Chill; Standard; Strict; Tough; Extreme; Impossible|]

    let create (judgement: Judgement) : Ruleset =

        let MARV, PERF, GREAT, GOOD, OKAY, MISS = judgement.Windows

        {
            Name = sprintf "Quaver %O" judgement
            Description = "Simulates Quaver's scoring system"
            Judgements =
                [|
                    {
                        Name = "Marvelous"
                        Color = Color.Aqua
                        TimingWindows = Some (-MARV, MARV)
                        BreaksCombo = false
                    }
                    {
                        Name = "Perfect"
                        Color = Color.Yellow
                        TimingWindows = Some (-PERF, PERF)
                        BreaksCombo = false
                    }
                    {
                        Name = "Great"
                        Color = Color.FromArgb(0, 255, 100)
                        TimingWindows = Some (-GREAT, GREAT)
                        BreaksCombo = false
                    }
                    // 50 and MISS have no late window
                    // Yes, this is not a mistake by me (maybe by peppy), the late window is 1ms shorter
                    // Hitting exactly the border of the 100 window does not hit the note
                    {
                        Name = "Good"
                        Color = Color.FromArgb(0, 160, 255)
                        TimingWindows = Some (-GOOD, GOOD)
                        BreaksCombo = false
                    }
                    {
                        Name = "Okay"
                        Color = Color.FromArgb(160, 160, 160)
                        TimingWindows = Some (-OKAY, OKAY)
                        BreaksCombo = false
                    }
                    {
                        Name = "Miss"
                        Color = Color.FromArgb(255, 80, 80)
                        TimingWindows = Some (-MISS, MISS)
                        BreaksCombo = true
                    }
                |]
            Grades =
                [|
                    {
                        Name = "D"
                        Accuracy = 0.6
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
                        Accuracy = 0.99
                        Color = Color.FromArgb(255, 255, 160)
                    }
                    {
                        Name = "X"
                        Accuracy = 1.0
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
                        Name = "1CB"
                        Requirement = LampRequirement.ComboBreaksAtMost 1
                        Color = Color.FromArgb(160, 160, 160)
                    }
                    {
                        Name = "FC"
                        Requirement = LampRequirement.ComboBreaksAtMost 0
                        Color = Color.FromArgb(0, 255, 160)
                    }
                    {
                        Name = "GFC"
                        Requirement = LampRequirement.JudgementAtMost (3, 0)
                        Color = Color.FromArgb(255, 255, 160)
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
                        Color = Color.FromArgb(255, 255, 160)
                    }
                    {
                        Name = "MFC"
                        Requirement = LampRequirement.JudgementAtMost (1, 0)
                        Color = Color.FromArgb(160, 255, 255)
                    }
                |]
            Accuracy = AccuracyPoints.PointsPerJudgement([| 1.0; 0.9825; 0.65; 0.25; -1.0; -0.5 |])
            HitMechanics =
                {
                    NotePriority = NotePriority.OsuMania
                    GhostTapJudgement = None
                }
            HoldMechanics =
                HoldMechanics.JudgeReleasesSeparately (
                    [|
                        Some (-MARV * 1.5f, MARV * 1.5f)
                        Some (-PERF * 1.5f, PERF * 1.5f)
                        Some (-GREAT * 1.5f, GREAT * 1.5f)
                        Some (-OKAY * 1.5f, OKAY * 1.5f)
                        None
                        None
                    |],
                    4
                )
            Formatting = { DecimalPlaces = DecimalPlaces.TWO }
        }