namespace Prelude.Gameplay.Rulesets

open Percyqaz.Common
open Prelude
open Prelude.Gameplay.Rulesets

module OsuMania =

    // Most info found from https://osu.ppy.sh/wiki/en/Gameplay/Judgement/osu!mania but it is not a thorough documentation of edge cases

    // osu! snaps all hit objects and all inputs to integer ms values, so your delta will always be an integer
    // The means a REAL LIFE +16.4.ms hit gets rounded down to 16 ms, and then 16 is in the border of 300g and 17 is out
    // So for equivalent behaviour on floating point time the windows used are with +0.5ms

    // In general the osu ruling is:
    // Windows actually act as quoted by the wiki, with no +0.5
    // round(delta) <= floor(window) with some exceptions that use <

    // Example: good window = 97.0 - 3.0 * od
    // On OD6.4 good window = 77.8
    // A hit at 77.4ms rounds to 77 <= floor(77.8)
    // A hit at 77.6ms rounds to 78 > floor(77.8)
    // So the OD6.4 good window includes 77 but not 78

    let perfect_window (_: float32) : GameplayTime = 16.0f<ms / rate>
    let great_window (od: float32) : GameplayTime = 64.0f<ms / rate> - 3.0f<ms / rate> * od
    let good_window (od: float32) : GameplayTime = 97.0f<ms / rate> - 3.0f<ms / rate> * od
    let ok_window (od: float32) : GameplayTime = 127.0f<ms / rate> - 3.0f<ms / rate> * od
    let meh_window (od: float32) : GameplayTime = 151.0f<ms / rate> - 3.0f<ms / rate> * od
    let miss_window (od: float32) : GameplayTime = 188.0f<ms / rate> - 3.0f<ms / rate> * od

    let to_hard_rock_window (unclipped_window: GameplayTime) = unclipped_window / 1.4f

    let to_easy_window (unclipped_window: GameplayTime) = unclipped_window * 1.4f

    let clipped (window: GameplayTime) = floor_uom window + 0.5f<ms / rate>

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
        let PERFECT = perfect_window od |> mode.Apply |> floor_uom
        let GREAT = great_window od |> mode.Apply |> floor_uom
        let GOOD = good_window od |> mode.Apply |> floor_uom
        let OK = ok_window od |> mode.Apply |> floor_uom
        let MEH = meh_window od |> mode.Apply |> floor_uom
        let MISS = miss_window od |> mode.Apply |> floor_uom

        {
            Window320 = clipped (PERFECT * 1.2f)
            Window300 = clipped (GREAT * 1.1f)
            Window200 = clipped GOOD
            Window100 = clipped OK
            Window50 = clipped MEH
            Window0 = clipped MISS
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

    let create (od: float32) (mode: Mode) : Ruleset =

        let od = round (od * 10.0f) / 10.0f
        if od < 0.0f || od > 10.0f then failwithf "Overall difficulty must be between 0 and 10, was: %.1f" od
        
        let PERFECT = perfect_window od |> mode.Apply |> clipped
        let GREAT = great_window od |> mode.Apply |> clipped
        let GOOD = good_window od |> mode.Apply |> clipped
        let OK = ok_window od |> mode.Apply |> clipped
        let MEH = meh_window od |> mode.Apply |> clipped
        let MISS = miss_window od |> mode.Apply |> clipped

        {
            Name = sprintf "osu! OD%s%s" (od.ToString("R")) (match mode with NoMod -> "" | Easy -> " +EZ" | HardRock -> " +HR")
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
                    // 50 and MISS have no late window
                    // Yes, this is not a mistake by me (maybe by peppy), the late window is 1ms shorter
                    // Hitting exactly the border of the 100 window does not hit the note
                    {
                        Name = "100"
                        Color = Color.FromArgb(0, 160, 255)
                        TimingWindows = Some (-OK, OK - 1.0f<ms / rate>)
                        BreaksCombo = false
                    }
                    {
                        Name = "50"
                        Color = Color.FromArgb(160, 160, 160)
                        TimingWindows = Some (-MEH, OK - 1.0f<ms / rate>)
                        BreaksCombo = false
                    }
                    {
                        Name = "MISS"
                        Color = Color.FromArgb(255, 80, 80)
                        TimingWindows = Some (-MISS, OK - 1.0f<ms / rate>)
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
            HitMechanics = 
                { 
                    NotePriority = NotePriority.OsuMania
                    GhostTapJudgement = None
                }
            HoldMechanics = HoldMechanics.CombineHeadAndTail (HeadTailCombineRule.OsuMania (ln_windows od mode))
            Formatting = { DecimalPlaces = DecimalPlaces.TWO }
        }