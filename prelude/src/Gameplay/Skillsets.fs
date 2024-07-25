namespace Prelude.Gameplay

open Percyqaz.Data
open Prelude
open Prelude.Charts.Processing

[<Json.AutoCodec>]
type PatternStatPoint = { BPM: int; Duration: Time }
type PatternStatLine = PatternStatPoint list

module PatternStatLine =

    let add (bpm: int, duration: Time) (stats: PatternStatLine) : PatternStatLine =
        let rec remove_worse_points (duration: Time) (bests: PatternStatLine) =
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

        if duration > 500.0f<ms> then loop stats else stats

    let rec get_duration_at (bpm: int) (stats: PatternStatLine) : Time option =
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

    let get_ratio (bpm: int, duration: Time) (stats: PatternStatLine) =
        match get_duration_at bpm stats with
        | None -> 10.0f
        | Some observed_duration -> duration / observed_duration |> min 10.0f |> max 0.1f

[<Json.AutoCodec>]
type PatternSkillBreakdown =
    {
        mutable Accuracy: PatternStatLine
        mutable Control: PatternStatLine
        mutable Normal: PatternStatLine
        mutable Survival: PatternStatLine
    }

    static member Default = { Accuracy = []; Control = []; Normal = []; Survival = [] }

module PatternSkillBreakdown =

    let multiplier (threshold: float) (accuracy: float) : float32 =
        if accuracy >= threshold then
            System.Math.Pow((1.0 - threshold) / (1.0 - accuracy), 0.3) |> float32
        else 
            System.Math.Pow((1.0 - threshold) / (1.0 - accuracy), 3.0) |> float32

    let private observe_octave pattern_type (density, accuracy, duration: Time) (breakdown: PatternSkillBreakdown) : unit =
        let feels_like_bpm =
            if pattern_type = Patterns.CorePatternType.Jack then
                density * 17.5f
            else density * 35f
            |> int

        match pattern_type with 
        | Patterns.CorePatternType.Jack ->
            breakdown.Accuracy <- breakdown.Accuracy |> PatternStatLine.add (feels_like_bpm, duration * multiplier 0.99 accuracy)
            breakdown.Control <- breakdown.Control |> PatternStatLine.add (feels_like_bpm, duration * multiplier 0.98 accuracy)
            breakdown.Normal <- breakdown.Normal |> PatternStatLine.add (feels_like_bpm, duration * multiplier 0.96 accuracy)
            breakdown.Survival <- breakdown.Survival |> PatternStatLine.add (feels_like_bpm, duration * multiplier 0.93 accuracy)
                        
        | Patterns.CorePatternType.Chordstream ->
            breakdown.Accuracy <- breakdown.Accuracy |> PatternStatLine.add (feels_like_bpm, duration * multiplier 0.985 accuracy)
            breakdown.Control <- breakdown.Control |> PatternStatLine.add (feels_like_bpm, duration * multiplier 0.97 accuracy)
            breakdown.Normal <- breakdown.Normal |> PatternStatLine.add (feels_like_bpm, duration * multiplier 0.945 accuracy)
            breakdown.Survival <- breakdown.Survival |> PatternStatLine.add (feels_like_bpm, duration * multiplier 0.91 accuracy)

        | Patterns.CorePatternType.Stream ->
            breakdown.Accuracy <- breakdown.Accuracy |> PatternStatLine.add (feels_like_bpm, duration * multiplier 0.98 accuracy)
            breakdown.Control <- breakdown.Control |> PatternStatLine.add (feels_like_bpm, duration * multiplier 0.965 accuracy)
            breakdown.Normal <- breakdown.Normal |> PatternStatLine.add (feels_like_bpm, duration * multiplier 0.93 accuracy)
            breakdown.Survival <- breakdown.Survival |> PatternStatLine.add (feels_like_bpm, duration * multiplier 0.90 accuracy)

    let private OCTAVES = 
        [|
            0.8f, 1.4f
            0.9f, 1.2f
            1.0f, 1.0f
            1.1f, 0.5f
            1.2f, 0.25f
            1.3f, 0.125f
        |]

    let observe pattern_type (density: float32, accuracy: float, duration: Time) (breakdown: PatternSkillBreakdown) : unit =
        for octave, time_mult in OCTAVES do
            observe_octave pattern_type (density * octave, accuracy, Time.of_number (duration * time_mult)) breakdown

[<Json.AutoCodec>]
type KeymodeSkillBreakdown =
    {
        Jack: PatternSkillBreakdown
        Chordstream: PatternSkillBreakdown
        Stream: PatternSkillBreakdown
    }

    static member Default =
        {
            Jack = PatternSkillBreakdown.Default
            Chordstream = PatternSkillBreakdown.Default
            Stream = PatternSkillBreakdown.Default
        }

module KeymodeSkillBreakdown =
    
    let observe pattern_type (density: float32, accuracy: float, duration: Time) (skills: KeymodeSkillBreakdown) : unit =
        match pattern_type with
        | Patterns.CorePatternType.Jack -> skills.Jack
        | Patterns.CorePatternType.Chordstream -> skills.Chordstream
        | Patterns.CorePatternType.Stream -> skills.Stream
        |> PatternSkillBreakdown.observe pattern_type (density, accuracy, duration)

    let expected_result (pattern_type: Patterns.CorePatternType, density: float32, duration: Time) (skills: KeymodeSkillBreakdown) =
        let target =
            match pattern_type with
            | Patterns.CorePatternType.Jack -> skills.Jack
            | Patterns.CorePatternType.Chordstream -> skills.Chordstream
            | Patterns.CorePatternType.Stream -> skills.Stream

        let feels_like_bpm =
            if pattern_type = Patterns.CorePatternType.Jack then
                density * 17.5f
            else density * 35f
            |> int

        if PatternStatLine.get_ratio (feels_like_bpm, duration) target.Accuracy < 1.0f then
            0.985
        elif PatternStatLine.get_ratio (feels_like_bpm, duration) target.Control < 1.0f then
            0.965
        elif PatternStatLine.get_ratio (feels_like_bpm, duration) target.Normal < 1.0f then
            0.945
        elif PatternStatLine.get_ratio (feels_like_bpm, duration) target.Survival < 1.0f then
            0.91
        else
            0.88
        |> fun x ->
            printfn "%s of %i bpm %A: %s" (duration |> format_duration_ms) (feels_like_bpm) pattern_type (format_accuracy x)
            x

    let query (patterns: Patterns.PatternInfo) (rate: float32) (skills: KeymodeSkillBreakdown) =
        let mutable total_weight : float = 0.1
        let mutable total : float = 0.0
        let add_weight res time =
            total <- total + res * time
            total_weight <- total_weight + time

        for p in patterns.Patterns do
            let time = 
                patterns.Patterns 
                |> Seq.filter (fun p2 -> p2.Pattern = p.Pattern && p2.BPM >= p.BPM && p2.Density50 > p.Density50)
                |> Seq.sumBy _.Amount

            let a1 = expected_result (p.Pattern, p.Density50 * rate, Time.of_number (time / rate)) skills
            add_weight a1 (time / rate * 1.0f |> float)
            let a2 = expected_result (p.Pattern, p.Density75 * rate, Time.of_number (time / rate * 0.5f)) skills
            add_weight a2 (time / rate * 0.5f |> float)
            let a3 = expected_result (p.Pattern, p.Density25 * rate, Time.of_number (time / rate * 1.5f)) skills
            add_weight a3 (time / rate * 1.5f |> float)

        total / total_weight