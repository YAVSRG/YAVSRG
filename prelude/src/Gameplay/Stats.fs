namespace Prelude.Gameplay

open Percyqaz.Data
open Prelude
open Prelude.Charts.Processing

[<Json.AutoCodec>]
type PatternStatPoint = { BPM: int; Duration: Time; Timestamp: int64 }
type PatternStatLine = PatternStatPoint list

module PatternStatLine =

    let add_observation (bpm: int, duration: Time, timestamp: int64) (stats: PatternStatLine) : PatternStatLine =
        let rec remove_worse_points (duration: Time) (bests: PatternStatLine) =
            match bests with
            | [] -> []
            | { Duration = time } :: xs when time <= duration -> remove_worse_points duration xs
            | xs -> xs

        let rec loop (xs: PatternStatLine) : PatternStatLine =
            match xs with
            | [] -> { BPM = bpm; Duration = duration; Timestamp = timestamp } :: []//, Improvement.New
            | existing :: xs ->
                if bpm < existing.BPM && duration > existing.Duration then
                    let res = loop xs in existing :: res//, imp
                elif bpm < existing.BPM then
                    existing :: xs//, Improvement.None
                elif bpm = existing.BPM && duration > existing.Duration then
                    { BPM = bpm; Duration = duration; Timestamp = timestamp } :: remove_worse_points duration xs//, Improvement.Better(value - v)
                elif bpm = existing.BPM then
                    existing :: xs//, Improvement.None
                else if duration >= existing.Duration then
                    { BPM = bpm; Duration = duration; Timestamp = timestamp } :: remove_worse_points duration xs//, Improvement.FasterBetter(rate - r, value - v)
                else
                    { BPM = bpm; Duration = duration; Timestamp = timestamp } :: existing :: xs//, Improvement.New

        if duration > 500.0f<ms> then loop stats else stats

[<Json.AutoCodec>]
type PatternSkillBreakdown =
    {
        mutable Accuracy: PatternStatLine
        mutable Control: PatternStatLine
        mutable Normal: PatternStatLine
        mutable Survival: PatternStatLine
    }

    static member Default = { Accuracy = []; Control = []; Normal = []; Survival = [] }

    member this.ObserveAccuracy(bpm: int, duration: Time, timestamp: int64) =
        this.Accuracy <- this.Accuracy |> PatternStatLine.add_observation (bpm, duration, timestamp)
    member this.ObserveControl(bpm: int, duration: Time, timestamp: int64) =
        this.Control <- this.Control |> PatternStatLine.add_observation (bpm, duration, timestamp)
    member this.ObserveNormal(bpm: int, duration: Time, timestamp: int64) =
        this.Normal <- this.Normal |> PatternStatLine.add_observation (bpm, duration, timestamp)
    member this.ObserveSurvival(bpm: int, duration: Time, timestamp: int64) =
        this.Survival <- this.Survival |> PatternStatLine.add_observation (bpm, duration, timestamp)

module PatternSkillBreakdown =

    let multiplier (threshold: float) (accuracy: float) : float32 =
        if accuracy >= threshold then 1.0f
        else System.Math.Pow((1.0 - threshold) / (1.0 - accuracy), 3.0) |> float32

    let observe pattern_type (density, accuracy, duration: Time, timestamp) (breakdown: PatternSkillBreakdown) : unit =
        let feels_like_bpm =
            if pattern_type = Patterns.CorePatternType.Jack then
                density * 17.5f
            else density * 35f
            |> int

        match pattern_type with 
        | Patterns.CorePatternType.Jack ->
            breakdown.ObserveAccuracy (feels_like_bpm, duration * multiplier 0.99 accuracy, timestamp)
            breakdown.ObserveControl (feels_like_bpm, duration * multiplier 0.98 accuracy, timestamp)
            breakdown.ObserveNormal (feels_like_bpm, duration * multiplier 0.96 accuracy, timestamp)
            breakdown.ObserveSurvival (feels_like_bpm, duration * multiplier 0.93 accuracy, timestamp)
                        
        | Patterns.CorePatternType.Chordstream ->
            breakdown.ObserveAccuracy (feels_like_bpm, duration * multiplier 0.985 accuracy, timestamp)
            breakdown.ObserveControl (feels_like_bpm, duration * multiplier 0.97 accuracy, timestamp)
            breakdown.ObserveNormal (feels_like_bpm, duration * multiplier 0.945 accuracy, timestamp)
            breakdown.ObserveSurvival (feels_like_bpm, duration * multiplier 0.91 accuracy, timestamp)

        | Patterns.CorePatternType.Stream ->
            breakdown.ObserveAccuracy (feels_like_bpm, duration * multiplier 0.98 accuracy, timestamp)
            breakdown.ObserveControl (feels_like_bpm, duration * multiplier 0.965 accuracy, timestamp)
            breakdown.ObserveNormal (feels_like_bpm, duration * multiplier 0.93 accuracy, timestamp)
            breakdown.ObserveSurvival (feels_like_bpm, duration * multiplier 0.90 accuracy, timestamp)