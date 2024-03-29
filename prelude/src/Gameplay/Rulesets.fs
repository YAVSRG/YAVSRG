﻿namespace Prelude.Gameplay

open System
open Percyqaz.Common
open Percyqaz.Data
open Prelude

type JudgementId = int

type AccuracySystemState =
    {
        Judgements: int array
        mutable PointsScored: float
        mutable MaxPointsScored: float
        mutable CurrentCombo: int
        mutable BestCombo: int
        mutable MaxPossibleCombo: int
        mutable ComboBreaks: int
    }
    member this.BreakCombo(would_have_increased_combo: bool) =
        if would_have_increased_combo then
            this.MaxPossibleCombo <- this.MaxPossibleCombo + 1

        this.CurrentCombo <- 0
        this.ComboBreaks <- this.ComboBreaks + 1

    member this.IncrCombo() =
        this.MaxPossibleCombo <- this.MaxPossibleCombo + 1
        this.CurrentCombo <- this.CurrentCombo + 1
        this.BestCombo <- max this.CurrentCombo this.BestCombo

    member this.Add(points: float, maxpoints: float, judge: JudgementId) =
        this.PointsScored <- this.PointsScored + points
        this.MaxPointsScored <- this.MaxPointsScored + maxpoints
        this.Judgements.[judge] <- this.Judgements.[judge] + 1

    member this.Add(judge: JudgementId) = this.Add(0.0, 0.0, judge)

/// Judgements are an indicator of how good a hit was, like "Perfect!" or "Nearly!"
/// Scores are commonly measured by how many of each judgement you get (for example a good score might be getting all "Perfect!" judgements)
[<Json.AutoCodec>]
type Judgement =
    {
        Name: string
        Color: Color
        BreaksCombo: bool
    }

/// Assignment of points per hit
/// Your total points are the sum of the points for each note you hit
/// Your % accuracy is number of points you get / max points possible
[<RequireQualifiedAccess>]
[<Json.AutoCodec>]
type AccuracyPoints =
    | WifeCurve of judge: int
    | Weights of maxweight: float * weights: float array
    member this.Validate name jcount =
        match this with
        | Weights(_, w) ->
            if w.Length <> jcount then
                Logging.Error(
                    sprintf
                        "Problem with ruleset '%s': %i accuracy weights given for %i judgements"
                        name
                        w.Length
                        jcount
                )

            this
        | _ -> this

/// Behaviour for hold notes
[<RequireQualifiedAccess>]
[<Json.AutoCodec>]
type HoldNoteBehaviour =
    | Osu of od: float32
    | JustBreakCombo
    | Normal of
        {|
            JudgementIfDropped: JudgementId
            JudgementIfOverheld: JudgementId
        |}
    | JudgeReleases of
        {|
            Timegates: (Time * JudgementId) list
        |}
    | OnlyJudgeReleases // uses base timegates
    member this.Validate name jcount =
        match this with
        | Normal d ->
            if d.JudgementIfDropped >= jcount || d.JudgementIfDropped < 0 then
                Logging.Error(sprintf "Problem with ruleset '%s': JudgementIfDropped is not a valid judgement" name)

            if d.JudgementIfOverheld >= jcount || d.JudgementIfOverheld < 0 then
                Logging.Error(sprintf "Problem with ruleset '%s': JudgementIfOverheld is not a valid judgement" name)

            Normal d
        | JudgeReleases d ->
            let mutable last_time = -Time.infinity

            for (time, j) in d.Timegates do
                if time <= last_time then
                    Logging.Error(sprintf "Problem with ruleset '%s': Release timegates are in the wrong order" name)

                if j >= jcount || j < 0 then
                    Logging.Error(sprintf "Problem with ruleset '%s': Release timegates judgement is not valid" name)

                last_time <- time

            JudgeReleases d
        | _ -> this

/// Grades are awarded at the end of a score as a summarising "rank" of how well you did
/// They typically follow lettering systems similar to academic exam grades
[<Json.AutoCodec>]
type Grade =
    {
        Name: string
        Accuracy: float
        Color: Color
    }

module Grade =

    type GradeResult =
        {
            Grade: int
            /// Improvement needed to get next grade; None if you got the best grade
            AccuracyNeeded: float option
        }

    let calculate_with_target (grades: Grade array) (percent: float) : GradeResult =

        let rec loop (achieved: int) =
            if achieved + 1 = grades.Length then // got max grade already
                {
                    Grade = achieved
                    AccuracyNeeded = None
                }
            else
                let accuracy_needed = grades.[achieved + 1].Accuracy - percent

                if accuracy_needed > 0.0 then
                    {
                        Grade = achieved
                        AccuracyNeeded = Some accuracy_needed
                    }
                else
                    loop (achieved + 1)

        loop -1

    let calculate (grades: Grade array) (state: AccuracySystemState) =
        (calculate_with_target grades (state.PointsScored / state.MaxPointsScored))
            .Grade

/// Lamps are awarded at the end of the score as a summarising "tag" to indicate certain accomplishments
/// Examples: You didn't miss a single note, so you get a "Full Combo" tag, you only got "Perfect" judgements, so you get a "Perfect Full Combo" tag
/// These provide alternative accomplishments to grades that can provide different challenge
[<Json.AutoCodec>]
type Lamp =
    {
        Name: string
        Judgement: JudgementId
        JudgementThreshold: int
        Color: Color
    }

module Lamp =

    type LampResult =
        {
            Lamp: int
            /// Improvement needed to get next lamp; None if you got the best lamp
            ImprovementNeeded:
                {|
                    Judgement: JudgementId
                    LessNeeded: int
                |} option
        }

    let calculate_with_target (lamps: Lamp array) (state: AccuracySystemState) : LampResult =

        let worst_judgement =
            let mutable w = -1
            let mutable i = 0

            while i < state.Judgements.Length do
                if state.Judgements.[i] > 0 then
                    w <- i

                i <- i + 1

            w

        let rec loop (achieved: int) =
            if achieved + 1 = lamps.Length then // got max grade already
                {
                    Lamp = achieved
                    ImprovementNeeded = None
                }
            else
                let next_lamp = lamps.[achieved + 1]

                if next_lamp.Judgement < 0 then // then it refers to cbs
                    if state.ComboBreaks > next_lamp.JudgementThreshold then
                        {
                            Lamp = achieved
                            ImprovementNeeded =
                                Some
                                    {|
                                        Judgement = -1
                                        LessNeeded = state.ComboBreaks - next_lamp.JudgementThreshold
                                    |}
                        }
                    else
                        loop (achieved + 1)
                else if worst_judgement > next_lamp.Judgement then
                    {
                        Lamp = achieved
                        ImprovementNeeded =
                            Some
                                {|
                                    Judgement = worst_judgement
                                    LessNeeded = state.Judgements.[worst_judgement]
                                |}
                    }
                elif state.Judgements.[next_lamp.Judgement] > next_lamp.JudgementThreshold then
                    {
                        Lamp = achieved
                        ImprovementNeeded =
                            Some
                                {|
                                    Judgement = next_lamp.Judgement
                                    LessNeeded = state.Judgements.[next_lamp.Judgement] - next_lamp.JudgementThreshold
                                |}
                    }
                else
                    loop (achieved + 1)

        loop -1

    let calculate (lamps: Lamp array) (state: AccuracySystemState) : int =
        (calculate_with_target lamps state).Lamp

[<Json.AutoCodec>]
type GradingConfig =
    {
        Grades: Grade array
        Lamps: Lamp array
    }
    member this.Validate jcount = this // todo: could validate lamps against judgements

[<Json.AutoCodec>]
type AccuracyConfig =
    {
        MissWindow: Time
        CbrushWindow: Time
        Timegates: (Time * JudgementId) list
        Points: AccuracyPoints
        HoldNoteBehaviour: HoldNoteBehaviour
    }
    member this.Validate name jcount =
        { this with
            Timegates =
                let mutable last_time = -Time.infinity

                for (time, j) in this.Timegates do
                    if time <= last_time then
                        Logging.Error(sprintf "Problem with ruleset '%s': Timegates are in the wrong order" name)

                    if j >= jcount || j < 0 then
                        Logging.Error(sprintf "Problem with ruleset '%s': Timegates judgement is not valid" name)

                    last_time <- time

                this.Timegates
            Points = this.Points.Validate name jcount
            HoldNoteBehaviour = this.HoldNoteBehaviour.Validate name jcount
        }

[<Json.AutoCodec>]
type Ruleset =
    {
        Name: string
        Description: string

        Judgements: Judgement array
        Accuracy: AccuracyConfig
        Grading: GradingConfig
    }
    member this.DefaultJudgement: JudgementId = this.Judgements.Length - 1

    member this.GradeName i =
        if i < 0 then "F"
        else if i >= this.Grading.Grades.Length then "??"
        else this.Grading.Grades.[i].Name

    member this.GradeColor i =
        if i < 0 || i >= this.Grading.Grades.Length then
            Color.Gray
        else
            this.Grading.Grades.[i].Color

    member this.LampName i =
        if i < 0 then "NONE"
        else if i >= this.Grading.Lamps.Length then "??"
        else this.Grading.Lamps.[i].Name

    member this.LampColor i =
        if i < 0 || i >= this.Grading.Grades.Length then
            Color.White
        else
            this.Grading.Lamps.[i].Color

    member this.JudgementName i = this.Judgements.[i].Name
    member this.JudgementColor i = this.Judgements.[i].Color

    member this.Validate =
        { this with
            Accuracy = this.Accuracy.Validate this.Name this.Judgements.Length
            Grading = this.Grading.Validate this.Judgements.Length
        }

module Ruleset =

    open System.IO
    open System.Security.Cryptography

    let hash (config: Ruleset) =
        let h = SHA256.Create()
        use ms = new MemoryStream()
        use bw = new BinaryWriter(ms)

        for j in config.Judgements do
            bw.Write j.BreaksCombo

        bw.Write(float32 config.Accuracy.MissWindow)
        bw.Write(float32 config.Accuracy.CbrushWindow)

        for t, j in config.Accuracy.Timegates do
            bw.Write(float32 t)
            bw.Write j

        match config.Accuracy.Points with
        | AccuracyPoints.WifeCurve j -> bw.Write j
        | AccuracyPoints.Weights(max, pts) ->
            bw.Write max

            for p in pts do
                bw.Write p

        match config.Accuracy.HoldNoteBehaviour with
        | HoldNoteBehaviour.Osu od -> bw.Write od
        | HoldNoteBehaviour.JustBreakCombo -> bw.Write 0s
        | HoldNoteBehaviour.Normal rules ->
            bw.Write rules.JudgementIfDropped
            bw.Write rules.JudgementIfOverheld
        | HoldNoteBehaviour.JudgeReleases d ->
            for t, j in d.Timegates do
                bw.Write(float32 t)
                bw.Write j
        | HoldNoteBehaviour.OnlyJudgeReleases -> bw.Write 1s

        for g in config.Grading.Grades do
            bw.Write g.Accuracy

        for l in config.Grading.Lamps do
            bw.Write l.Judgement
            bw.Write l.JudgementThreshold

        let s = ms.ToArray() |> h.ComputeHash |> BitConverter.ToString
        config.Name.Replace(" ", "") + s.Replace("-", "").Substring(0, 6)

module RulesetUtils =

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

    let osu_ln_judgement
        (od: float32)
        (head_delta: Time)
        (tail_delta: Time)
        (overhold: bool)
        (dropped: bool)
        : JudgementId =
        let tail_delta_abs = Time.abs tail_delta
        let head_delta_abs = Time.abs head_delta

        if not dropped && not overhold then

            let window_320 = 16.5f<ms> * 1.2f
            let window_300 = (64.5f<ms> - od * 3.0f<ms>) * 1.1f
            let window_200 = 97.5f<ms> - od * 3.0f<ms>
            let window_100 = 127.5f<ms> - od * 3.0f<ms>
            let window_50 = 151.5f<ms> - od * 3.0f<ms>

            let mean = (tail_delta_abs + head_delta_abs) * 0.5f

            if tail_delta < -window_50 then 5

            elif head_delta_abs < window_320 && mean < window_320 then 0 // 300g
            elif head_delta_abs < window_300 && mean < window_300 then 1 // 300
            elif head_delta_abs < window_200 && mean < window_200 then 2 // 200
            elif head_delta_abs < window_100 && mean < window_100 then 3 // 100
            elif head_delta_abs < window_50 && mean < window_50 then 4 // 50
            else 5 // miss

        elif dropped then

            let window_hit = 151.5f<ms> - od * 3.0f<ms>
            if tail_delta_abs < window_hit then 4 else 5

        else
            let window_overhold_200 = 42.5f<ms> - od * 3.0f<ms>
            let window_overhold_100 = 103.5f<ms> - od * 3.0f<ms>

            if head_delta_abs < window_overhold_200 then 2 // 200
            elif head_delta_abs < window_overhold_100 then 3 // 100
            else 4 // 50

module PrefabRulesets =

    module DP =

        let windows judge ridiculous =
            let pf = 45.0f<ms> / 6.0f * (10.0f - (judge |> float32))

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

    module Wife =

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
                                    Color = Color.Gold
                                }
                                {
                                    Name = "AAAA."
                                    Accuracy = 0.9997
                                    Color = Color.Gold
                                }
                                {
                                    Name = "AAAA:"
                                    Accuracy = 0.9998
                                    Color = Color.Gold
                                }
                                {
                                    Name = "AAAAA"
                                    Accuracy = 0.999935
                                    Color = Color.Gold
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

    module Osu =

        let windows od =
            let ma = 16.5f<ms>
            let pf = 64.5f<ms> - od * 3.0f<ms>
            let gr = 97.5f<ms> - od * 3.0f<ms>
            let gd = 127.5f<ms> - od * 3.0f<ms>
            let bd = 151.5f<ms> - od * 3.0f<ms>
            [ -bd, 5; -gd, 4; -gr, 3; -pf, 2; -ma, 1; ma, 0; pf, 1; gr, 2; gd, 3; bd, 4 ]

        let create (od: float32) : Ruleset =
            {
                Name = sprintf "osu! (OD%.1f)" od
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
                        MissWindow = 188.0f<ms> - od * 3.0f<ms>
                        CbrushWindow = 151.5f<ms> - od * 3.0f<ms>
                        Timegates = windows od
                        Points = AccuracyPoints.Weights(300.0, [| 300.0; 300.0; 200.0; 100.0; 50.0; 0.0 |])
                        HoldNoteBehaviour = HoldNoteBehaviour.Osu od
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

        let create (judge: int) =
            {
                Name = sprintf "SC (J%i)" judge
                Description = "The 'official' scoring system of Interlude, fine tuned for maximum fun"
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
                        Points = AccuracyPoints.Weights(10.0, [| 10.0; 9.0; 5.0; -5.0; -10.0; -10.0 |])
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

    [<Json.AutoCodec>]
    type Repo =
        {
            Rulesets: Collections.Generic.Dictionary<string, Ruleset>
        }
