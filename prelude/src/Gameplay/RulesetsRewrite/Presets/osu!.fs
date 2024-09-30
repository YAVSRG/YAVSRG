namespace Prelude.Gameplay.RulesetsV2

open Prelude
open Prelude.Gameplay.RulesetsV2

module OsuMania =

    // Most info found from https://osu.ppy.sh/wiki/en/Gameplay/Judgement/osu!mania but it is not a thorough documentation of edge cases
    // The floor + 0.5ms stuff is exact as found through experimental evidence with the osu! stable client

    // osu! snaps all hit objects and all inputs to integer ms values, so your delta will always be an integer
    // the +0.5 on hit windows make it very clear that landing EXACTLY on the integer hit window will count as that judgement
    // e.g. on OD0, an exactly +64ms hit IS inside the great window and gives you a yellow 300

    let perfect_window (_: float32) : GameplayTime = 16.5f<ms / rate>
    let great_window (od: float32) : GameplayTime = floor (64.0f - 3.0f * od) * 1.0f<ms / rate> + 0.5f<ms / rate>
    let good_window (od: float32) : GameplayTime = floor (97.0f - 3.0f * od) * 1.0f<ms / rate> + 0.5f<ms / rate>
    let ok_window (od: float32) : GameplayTime = floor (127.0f - 3.0f * od) * 1.0f<ms / rate> + 0.5f<ms / rate>
    let meh_window (od: float32) : GameplayTime = floor (151.0f - 3.0f * od) * 1.0f<ms / rate> + 0.5f<ms / rate>
    let miss_window (od: float32) : GameplayTime = floor (188.0f - 3.0f * od) * 1.0f<ms / rate> + 0.5f<ms / rate>

    let to_hard_rock_window (window: GameplayTime) =
        floor (5f / 7f<ms / rate> * window) * 1.0f<ms / rate> + 0.5f<ms / rate>

    let to_easy_window (window: GameplayTime) =
        floor (7f / 5f<ms / rate> * window) * 1.0f<ms / rate> + 0.5f<ms / rate>

    type Mode =
        | Easy
        | NoMod
        | HardRock
        member this.Apply =
            match this with
            | Easy -> to_easy_window
            | NoMod -> id
            | HardRock -> to_hard_rock_window

    let private ln_windows (od: float32) (mode: Mode) : OsuLnWindows =
        let PERFECT = perfect_window od |> mode.Apply
        let GREAT = great_window od |> mode.Apply
        let GOOD = good_window od |> mode.Apply
        let OK = ok_window od |> mode.Apply
        let MEH = meh_window od |> mode.Apply

        {
            Window320 = floor (PERFECT * 1.2f<rate / ms>) * 1.0f<ms / rate> + 0.5f<ms / rate>
            Window300 = floor (GREAT * 1.1f<rate / ms>) * 1.0f<ms / rate> + 0.5f<ms / rate>
            Window200 = GOOD
            Window100 = OK
            Window50 = MEH
            // todo: this matches experimental data but I must not be understanding something about LNs
            WindowOverhold200 = 
                let base_window = floor (43f - od * 3f) * 1.0f<ms / rate> + 0.5f<ms / rate>
                match mode with
                | Easy -> floor (base_window * 1.4f<rate / ms>) * 1.0f<ms / rate> - 0.5f<ms / rate>
                | HardRock -> floor (base_window / 1.4f<ms / rate>) * 1.0f<ms / rate> + 1.5f<ms / rate>
                | NoMod -> base_window
            WindowOverhold100 =
                let base_window = floor (103f - od * 3f) * 1.0f<ms / rate> + 0.5f<ms / rate>
                match mode with
                | Easy -> floor (base_window * 1.4f<rate / ms>) * 1.0f<ms / rate> - 0.5f<ms / rate>
                | HardRock -> floor (base_window / 1.4f<ms / rate>) * 1.0f<ms / rate> + 1.5f<ms / rate>
                | NoMod -> base_window
        }

    let create (od: float32) (mode: Mode) : RulesetV2 =
        
        let PERFECT = perfect_window od |> mode.Apply
        let GREAT = great_window od |> mode.Apply
        let GOOD = good_window od |> mode.Apply
        let OK = ok_window od |> mode.Apply
        let MEH = meh_window od |> mode.Apply
        let MISS = miss_window od |> mode.Apply

        {
            Name = sprintf "osu! (OD%.1f%s)" od (match mode with NoMod -> "" | Easy -> " +EZ" | HardRock -> " +HR")
            Description = "Simulates osu!'s scoring system"
            Judgements =
                [|
                    {
                        Name = "300g"
                        Color = Color.Aqua
                        TimingWindows = Some (-PERFECT, PERFECT)
                        BreaksCombo = false
                    }
                    {
                        Name = "300"
                        Color = Color.Yellow
                        TimingWindows = Some (-GREAT, GREAT)
                        BreaksCombo = false
                    }
                    {
                        Name = "200"
                        Color = Color.FromArgb(0, 255, 100)
                        TimingWindows = Some (-GOOD, GOOD)
                        BreaksCombo = false
                    }
                    {
                        Name = "100"
                        Color = Color.FromArgb(0, 160, 255)
                        TimingWindows = Some (-OK, OK)
                        BreaksCombo = false
                    }
                    {
                        Name = "50"
                        Color = Color.FromArgb(160, 160, 160)
                        TimingWindows = Some (-MEH, OK) // 50 and Miss have no late window
                        BreaksCombo = false
                    }
                    {
                        Name = "MISS"
                        Color = Color.FromArgb(255, 80, 80)
                        TimingWindows = Some (-MISS, OK)
                        BreaksCombo = true
                    }
                |]
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
                        Name = "PERF"
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
                        Name = "SS"
                        Requirement = LampRequirement.JudgementAtMost (2, 0)
                        Color = Color.FromArgb(255, 255, 160)
                    }
                    {
                        Name = "MILL"
                        Requirement = LampRequirement.JudgementAtMost (1, 0)
                        Color = Color.FromArgb(160, 255, 255)
                    }
                |]
            Accuracy = AccuracyPoints.PointsPerJudgement([| 1.0; 1.0; 2.0/3.0; 1.0/3.0; 1.0/6.0; 0.0 |])
            HitMechanics = HitMechanics.OsuMania
            HoldMechanics = HoldMechanics.CombineHeadAndTail (HeadTailCombineRule.OsuMania (ln_windows od mode))
        }