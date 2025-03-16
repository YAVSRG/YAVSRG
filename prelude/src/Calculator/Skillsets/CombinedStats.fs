namespace Prelude.Calculator

open Percyqaz.Data
open Prelude

[<Json.AutoCodec>]
type CombinedStatPoint = { Density: int; Duration: GameplayTime }
type CombinedStatLine = CombinedStatPoint list

module CombinedStatLine =

    let add (density: int, duration: GameplayTime) (stats: CombinedStatLine) : CombinedStatLine =
        let rec remove_worse_points (duration: GameplayTime) (bests: CombinedStatLine) =
            match bests with
            | [] -> []
            | { Duration = time } :: xs when time <= duration -> remove_worse_points duration xs
            | xs -> xs

        let rec loop (xs: CombinedStatLine) : CombinedStatLine =
            match xs with
            | [] -> { Density = density; Duration = duration } :: []//, Improvement.New
            | existing :: xs ->
                if density < existing.Density && duration > existing.Duration then
                    let res = loop xs in existing :: res//, imp
                elif density < existing.Density then
                    existing :: xs//, Improvement.None
                elif density = existing.Density && duration > existing.Duration then
                    { Density = density; Duration = duration } :: remove_worse_points duration xs//, Improvement.Better(value - v)
                elif density = existing.Density then
                    existing :: xs//, Improvement.None
                else if duration >= existing.Duration then
                    { Density = density; Duration = duration } :: remove_worse_points duration xs//, Improvement.FasterBetter(rate - r, value - v)
                else
                    { Density = density; Duration = duration } :: existing :: xs//, Improvement.New

        if duration > 500.0f<ms / rate> then loop stats else stats

    let rec get_duration_at (bpm: int) (stats: CombinedStatLine) : GameplayTime option =
        match stats with
        | [] -> None
        | x :: xs ->
            if x.Density = bpm then
                Some x.Duration
            elif x.Density < bpm then
                None
            else
                get_duration_at bpm xs
                |> function
                    | None -> Some x.Duration
                    | Some better_duration -> Some better_duration

    let value (stats: CombinedStatLine) =
        let ONE_MINUTE = 60000f<ms / rate>
        let duration_value (duration: GameplayTime) =
            if duration > ONE_MINUTE then
                1.0f + ((duration - ONE_MINUTE) / ONE_MINUTE * 0.01f)
            else duration / ONE_MINUTE

        let rec v (xs: CombinedStatLine) =
            match xs with
            | x :: y :: xs ->
                (float32 x.Density - float32 y.Density) * duration_value x.Duration
                + v (y :: xs)
            | x :: [] ->
                float32 x.Density * duration_value x.Duration
            | [] -> 0.0f
        v stats * 150.0f

    let scale (multiplier: float32) (stats: CombinedStatLine) : CombinedStatLine =
        stats |> List.map (fun { Density = d; Duration = duration } -> { Density = d; Duration = duration * multiplier })

type CombinedSkillIncrease =
    {
        Accuracy: float32
        Control: float32
        Push: float32
    }
    member this.Total = this.Accuracy + this.Control + this.Push
    override this.ToString() = sprintf "+%.0f Accuracy, +%.0f Control, +%.0f Push (+%.0f Total)" this.Accuracy this.Control this.Push this.Total

[<Json.AutoCodec>]
type CombinedSkillBreakdown =
    {
        mutable Accuracy: CombinedStatLine
        mutable Control: CombinedStatLine
        mutable Push: CombinedStatLine
    }

    static member Default = { Accuracy = []; Control = []; Push = [] }
    member this.Copy = { Accuracy = this.Accuracy; Control = this.Control; Push = this.Push }
    member this.CompareImprovement (other: CombinedSkillBreakdown) : CombinedSkillIncrease =
        {
            Accuracy = CombinedStatLine.value this.Accuracy - CombinedStatLine.value other.Accuracy
            Control = CombinedStatLine.value this.Control - CombinedStatLine.value other.Control
            Push = CombinedStatLine.value this.Push - CombinedStatLine.value other.Push
        }
    member this.Scale (multiplier: float32) =
        {
            Accuracy = CombinedStatLine.scale multiplier this.Accuracy
            Control = CombinedStatLine.scale multiplier this.Control
            Push = CombinedStatLine.scale multiplier this.Push
        }

module CombinedSkillBreakdown =

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

    let private observe_octave (density: float32, accuracy, duration: GameplayTime) (breakdown: CombinedSkillBreakdown) : unit =
        let density = int density

        breakdown.Accuracy <- breakdown.Accuracy |> CombinedStatLine.add (density, duration * multiplier 0.985 accuracy)
        breakdown.Control <- breakdown.Control |> CombinedStatLine.add (density, duration * multiplier 0.95 accuracy)
        breakdown.Push <- breakdown.Push |> CombinedStatLine.add (density, duration * multiplier 0.92 accuracy)

    let observe (density: float32, accuracy: float, duration: GameplayTime) (breakdown: CombinedSkillBreakdown) : unit =
        for octave, time_mult in OCTAVES do
            observe_octave (density * octave, accuracy, duration * time_mult) breakdown