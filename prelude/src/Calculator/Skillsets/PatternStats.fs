namespace Prelude.Calculator

open Percyqaz.Data
open Prelude

[<Json.AutoCodec>]
type PatternStatPoint = { BPM: int; Duration: GameplayTime }
type PatternStatLine = PatternStatPoint list

module PatternStatLine =

    let add (bpm: int, duration: GameplayTime) (stats: PatternStatLine) : PatternStatLine =
        let rec remove_worse_points (duration: GameplayTime) (bests: PatternStatLine) =
            match bests with
            | [] -> []
            | { Duration = time } :: xs when time <= duration -> remove_worse_points duration xs
            | xs -> xs

        let rec loop (xs: PatternStatLine) : PatternStatLine =
            match xs with
            | [] -> { BPM = bpm; Duration = duration } :: []//, Improvement.New
            | existing :: xs ->
                if bpm < existing.BPM && duration > existing.Duration then
                    let res = loop xs in existing :: res//, imp
                elif bpm < existing.BPM then
                    existing :: xs//, Improvement.None
                elif bpm = existing.BPM && duration > existing.Duration then
                    { BPM = bpm; Duration = duration } :: remove_worse_points duration xs//, Improvement.Better(value - v)
                elif bpm = existing.BPM then
                    existing :: xs//, Improvement.None
                else if duration >= existing.Duration then
                    { BPM = bpm; Duration = duration } :: remove_worse_points duration xs//, Improvement.FasterBetter(rate - r, value - v)
                else
                    { BPM = bpm; Duration = duration } :: existing :: xs//, Improvement.New

        if duration > 500.0f<ms / rate> then loop stats else stats

    let rec get_duration_at (bpm: int) (stats: PatternStatLine) : GameplayTime option =
        match stats with
        | [] -> None
        | x :: xs ->
            if x.BPM = bpm then
                Some x.Duration
            elif x.BPM < bpm then
                None
            else
                get_duration_at bpm xs
                |> function
                    | None -> Some x.Duration
                    | Some better_duration -> Some better_duration

    let value (stats: PatternStatLine) : float32 =
        let ONE_MINUTE = 60000f<ms / rate>
        let duration_value (duration: GameplayTime) =
            if duration > ONE_MINUTE then
                1.0f + ((duration - ONE_MINUTE) / ONE_MINUTE * 0.01f)
            else duration / ONE_MINUTE

        let rec v (xs: PatternStatLine) =
            match xs with
            | x :: y :: xs ->
                (float32 x.BPM - float32 y.BPM) * duration_value x.Duration
                + v (y :: xs)
            | x :: [] ->
                float32 x.BPM * duration_value x.Duration
            | [] -> 0.0f
        v stats * 10.0f

    let scale (multiplier: float32) (stats: PatternStatLine) : PatternStatLine =
        stats |> List.map (fun { BPM = bpm; Duration = duration } -> { BPM = bpm; Duration = duration * multiplier })

type PatternSkillIncrease =
    {
        Accuracy: float32
        Control: float32
        Push: float32
    }
    member this.Total = this.Accuracy + this.Control + this.Push
    override this.ToString() = sprintf "+%.0f Accuracy, +%.0f Control, +%.0f Push (+%.0f Total)" this.Accuracy this.Control this.Push this.Total

[<Json.AutoCodec>]
type PatternSkillBreakdown =
    {
        mutable Accuracy: PatternStatLine
        mutable Control: PatternStatLine
        mutable Push: PatternStatLine
    }

    static member Default = { Accuracy = []; Control = []; Push = [] }
    member this.Copy = { Accuracy = this.Accuracy; Control = this.Control; Push = this.Push }
    member this.CompareImprovement (mult: float32) (other: PatternSkillBreakdown) : PatternSkillIncrease =
        {
            Accuracy = (PatternStatLine.value this.Accuracy - PatternStatLine.value other.Accuracy) * mult
            Control = (PatternStatLine.value this.Control - PatternStatLine.value other.Control) * mult
            Push = (PatternStatLine.value this.Push - PatternStatLine.value other.Push) * mult
        }
    member this.Scale (multiplier: float32) =
        {
            Accuracy = PatternStatLine.scale multiplier this.Accuracy
            Control = PatternStatLine.scale multiplier this.Control
            Push = PatternStatLine.scale multiplier this.Push
        }

module PatternSkillBreakdown =

    let multiplier (threshold: float) (accuracy: float) : float32 =
        if accuracy >= threshold then
            System.Math.Pow((1.0 - threshold) / (max (1.0 - accuracy) 0.001), 0.3) |> float32
        else
            System.Math.Pow((1.0 - threshold) / (1.0 - accuracy), 3.0) |> float32

    let private OCTAVES =
        [|
            0.8f, 1.4f
            0.9f, 1.2f
            1.0f, 1.0f
            1.1f, 0.5f
            1.2f, 0.25f
            1.3f, 0.125f
        |]

    let private observe_octave (pattern_type: Patterns.CorePattern) (density, accuracy, duration: GameplayTime) (breakdown: PatternSkillBreakdown) : unit =
        let feels_like_bpm = pattern_type.DensityToBPM * density |> int

        let acc_threshold, ctrl_threshold, push_threshold = pattern_type.AccuracyBreakpoints

        breakdown.Accuracy <- breakdown.Accuracy |> PatternStatLine.add (feels_like_bpm, duration * multiplier acc_threshold accuracy)
        breakdown.Control <- breakdown.Control |> PatternStatLine.add (feels_like_bpm, duration * multiplier ctrl_threshold accuracy)
        breakdown.Push <- breakdown.Push |> PatternStatLine.add (feels_like_bpm, duration * multiplier push_threshold accuracy)

    let observe pattern_type (density: float32, accuracy: float, duration: GameplayTime) (breakdown: PatternSkillBreakdown) : unit =
        for octave, time_mult in OCTAVES do
            observe_octave pattern_type (density * octave, accuracy, duration * time_mult) breakdown