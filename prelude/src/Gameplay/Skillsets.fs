namespace Prelude.Gameplay

open Percyqaz.Data
open Prelude
open Prelude.Charts.Processing
open Prelude.Charts.Processing.Patterns

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

    let value (stats: PatternStatLine) =
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
    member this.Minus (other: PatternSkillBreakdown) : PatternSkillIncrease =
        {
            Accuracy = PatternStatLine.value this.Accuracy - PatternStatLine.value other.Accuracy
            Control = PatternStatLine.value this.Control - PatternStatLine.value other.Control
            Push = PatternStatLine.value this.Push - PatternStatLine.value other.Push
        }

module PatternSkillBreakdown =

    let multiplier (threshold: float) (accuracy: float) : float32 =
        if accuracy >= threshold then
            System.Math.Pow((1.0 - threshold) / (max (1.0 - accuracy) 0.001), 0.3) |> float32
        else 
            System.Math.Pow((1.0 - threshold) / (1.0 - accuracy), 3.0) |> float32

    let private observe_octave (pattern_type: Patterns.CorePattern) (density, accuracy, duration: GameplayTime) (breakdown: PatternSkillBreakdown) : unit =
        let feels_like_bpm = pattern_type.DensityToBPM * density |> int

        let acc_threshold, ctrl_threshold, push_threshold = pattern_type.AccuracyBreakpoints

        breakdown.Accuracy <- breakdown.Accuracy |> PatternStatLine.add (feels_like_bpm, duration * multiplier acc_threshold accuracy)
        breakdown.Control <- breakdown.Control |> PatternStatLine.add (feels_like_bpm, duration * multiplier ctrl_threshold accuracy)
        breakdown.Push <- breakdown.Push |> PatternStatLine.add (feels_like_bpm, duration * multiplier push_threshold accuracy)

    let private OCTAVES = 
        [|
            0.8f, 1.4f
            0.9f, 1.2f
            1.0f, 1.0f
            1.1f, 0.5f
            1.2f, 0.25f
            1.3f, 0.125f
        |]

    let observe pattern_type (density: float32, accuracy: float, duration: GameplayTime) (breakdown: PatternSkillBreakdown) : unit =
        for octave, time_mult in OCTAVES do
            observe_octave pattern_type (density * octave, accuracy, duration * time_mult) breakdown

type KeymodeSkillIncrease =
    {
        Jack: PatternSkillIncrease
        Chordstream: PatternSkillIncrease
        Stream: PatternSkillIncrease
    }
    member this.Total = this.Jack.Total + this.Chordstream.Total + this.Stream.Total
    override this.ToString() = 
        if this.Total = 0.0f then "No change" else
        [
            if this.Jack.Total > 0.0f then sprintf "Jack: %O" this.Jack else "Jack: --"
            if this.Chordstream.Total > 0.0f then sprintf "Chordstream: %O" this.Chordstream else "Chordstream: --"
            if this.Stream.Total > 0.0f then sprintf "Stream: %O" this.Stream else "Stream: --"
            sprintf "+%.0f Total" this.Total
        ]
        |> String.concat "\n"

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
    
    member this.Copy = { Jack = this.Jack.Copy; Chordstream = this.Chordstream.Copy; Stream = this.Stream.Copy }
    member this.Minus (other: KeymodeSkillBreakdown) : KeymodeSkillIncrease =
        {
            Jack = this.Jack.Minus other.Jack
            Chordstream = this.Chordstream.Minus other.Chordstream
            Stream = this.Stream.Minus other.Stream
        }

module KeymodeSkillBreakdown =

    let score (patterns: Cluster array) (accuracy: float) (rate: Rate) (skills: KeymodeSkillBreakdown) : KeymodeSkillIncrease =
        
        let before = skills.Copy

        for p in patterns |> Seq.where (fun p -> p.SpecificType.IsNone) do

            let time = 
                patterns 
                |> Seq.filter (fun p2 -> p2.Pattern = p.Pattern && p2.BPM >= p.BPM && p2.Density50 >= p.Density50)
                |> Seq.sumBy _.Amount

            let skill =
                match p.Pattern with
                | Jack -> skills.Jack
                | Chordstream -> skills.Chordstream
                | Stream -> skills.Stream
                
            PatternSkillBreakdown.observe p.Pattern (p.Density10 * rate, accuracy, time / rate * 1.8f) skill
            PatternSkillBreakdown.observe p.Pattern (p.Density25 * rate, accuracy, time / rate * 1.5f) skill
            PatternSkillBreakdown.observe p.Pattern (p.Density50 * rate, accuracy, time / rate) skill
            PatternSkillBreakdown.observe p.Pattern (p.Density75 * rate, accuracy, time / rate * 0.5f) skill
            PatternSkillBreakdown.observe p.Pattern (p.Density90 * rate, accuracy, time / rate * 0.2f) skill

        skills.Minus before

    let what_if (patterns: Cluster array) (accuracy: float) (rate: Rate) (skills: KeymodeSkillBreakdown) : KeymodeSkillIncrease =

        let potential = skills.Copy

        for p in patterns |> Seq.where (fun p -> p.SpecificType.IsNone) do
            let time = 
                patterns 
                |> Seq.filter (fun p2 -> p2.Pattern = p.Pattern && p2.BPM >= p.BPM && p2.Density50 >= p.Density50)
                |> Seq.sumBy _.Amount

            let skill =
                match p.Pattern with
                | Jack -> potential.Jack
                | Chordstream -> potential.Chordstream
                | Stream -> potential.Stream

            PatternSkillBreakdown.observe p.Pattern (p.Density10 * rate, accuracy, time / rate * 1.8f) skill
            PatternSkillBreakdown.observe p.Pattern (p.Density25 * rate, accuracy, time / rate * 1.5f) skill
            PatternSkillBreakdown.observe p.Pattern (p.Density50 * rate, accuracy, time / rate) skill
            PatternSkillBreakdown.observe p.Pattern (p.Density75 * rate, accuracy, time / rate * 0.5f) skill
            PatternSkillBreakdown.observe p.Pattern (p.Density90 * rate, accuracy, time / rate * 0.2f) skill

        potential.Minus skills