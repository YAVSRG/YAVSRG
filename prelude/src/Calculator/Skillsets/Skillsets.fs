namespace Prelude.Calculator

open Percyqaz.Data
open Prelude
open Prelude.Calculator.Patterns

type KeymodeSkillIncrease =
    {
        Jacks: PatternSkillIncrease
        Chordstream: PatternSkillIncrease
        Stream: PatternSkillIncrease
        Combined: CombinedSkillIncrease
    }
    member this.Total = this.Jacks.Total + this.Chordstream.Total + this.Stream.Total + this.Combined.Total
    override this.ToString() =
        if this.Total = 0.0f then "No change" else
        [
            if this.Jacks.Total > 0.0f then sprintf "Jacks: %O" this.Jacks else "Jacks: --"
            if this.Chordstream.Total > 0.0f then sprintf "Chordstream: %O" this.Chordstream else "Chordstream: --"
            if this.Stream.Total > 0.0f then sprintf "Stream: %O" this.Stream else "Stream: --"
            if this.Combined.Total > 0.0f then sprintf "Combined: %O" this.Combined else "Combined: --"
            sprintf "Total: +%.0f" this.Total
        ]
        |> String.concat "\n"

[<Json.AutoCodec>]
type KeymodeTinyBreakdown =
    {
        Jacks: float32
        Chordstream: float32
        Stream: float32
        Combined: float32
    }

    member this.Average = (this.Chordstream + this.Jacks + this.Stream) / 3f

    static member Default =
        {
            Jacks = 0.0f
            Chordstream = 0.0f
            Stream = 0.0f
            Combined = 0.0f
        }

[<Json.AutoCodec>]
type KeymodeSkillBreakdown =
    {
        Jacks: PatternSkillBreakdown
        Chordstream: PatternSkillBreakdown
        Stream: PatternSkillBreakdown
        Combined: CombinedSkillBreakdown
    }

    static member Default =
        {
            Jacks = PatternSkillBreakdown.Default
            Chordstream = PatternSkillBreakdown.Default
            Stream = PatternSkillBreakdown.Default
            Combined = CombinedSkillBreakdown.Default
        }

    member this.Copy = { Jacks = this.Jacks.Copy; Chordstream = this.Chordstream.Copy; Stream = this.Stream.Copy; Combined = this.Combined.Copy }

    member this.Minus (other: KeymodeSkillBreakdown) : KeymodeSkillIncrease =
        {
            Jacks = this.Jacks.CompareImprovement CorePattern.Jacks.RatingMultiplier other.Jacks
            Chordstream = this.Chordstream.CompareImprovement CorePattern.Chordstream.RatingMultiplier other.Chordstream
            Stream = this.Stream.CompareImprovement CorePattern.Stream.RatingMultiplier other.Stream
            Combined = this.Combined.CompareImprovement other.Combined
        }

    member this.Scale (multiplier: float32) =
        {
            Jacks = this.Jacks.Scale multiplier
            Chordstream = this.Chordstream.Scale multiplier
            Stream = this.Stream.Scale multiplier
            Combined = this.Combined.Scale multiplier
        }

    member this.Tiny : KeymodeTinyBreakdown =
        {
            Jacks = this.Jacks.CompareImprovement CorePattern.Jacks.RatingMultiplier PatternSkillBreakdown.Default |> _.Total
            Chordstream = this.Chordstream.CompareImprovement CorePattern.Chordstream.RatingMultiplier PatternSkillBreakdown.Default |> _.Total
            Stream = this.Stream.CompareImprovement CorePattern.Stream.RatingMultiplier PatternSkillBreakdown.Default |> _.Total
            Combined = this.Combined.CompareImprovement CombinedSkillBreakdown.Default |> _.Total
        }

module KeymodeSkillBreakdown =

    let private COMBINED_MULTIPLIER_SCALE_CONSTANT = sqrt 2.0f
    let private DAILY_DECAY_CONSTANT = -0.105360515

    let decay_over_time (before: int64) (now: int64) : float32 =
        let days = float (now - before) / 86_400_000.0
        exp (DAILY_DECAY_CONSTANT * days) |> float32

    let private skill_increase (patterns: PatternReport) (accuracy: float) (rate: Rate) (skills: KeymodeSkillBreakdown) : unit =

        let total_stream = patterns.Clusters |> Seq.where(fun c -> c.Pattern = Stream) |> Seq.sumBy _.Amount
        let total_chordstream = patterns.Clusters |> Seq.where(fun c -> c.Pattern = Chordstream) |> Seq.sumBy _.Amount
        let total_jacks = patterns.Clusters |> Seq.where(fun c -> c.Pattern = Jacks) |> Seq.sumBy _.Amount

        let combined_multiplier =
            (
                sqrt (total_stream / patterns.Duration)
                + sqrt (total_chordstream / patterns.Duration)
                + sqrt (total_jacks / patterns.Duration)
            ) / COMBINED_MULTIPLIER_SCALE_CONSTANT

        CombinedSkillBreakdown.observe (patterns.Density10 * rate * combined_multiplier, accuracy, patterns.Duration / rate * 1.8f) skills.Combined
        CombinedSkillBreakdown.observe (patterns.Density25 * rate * combined_multiplier, accuracy, patterns.Duration / rate * 1.5f) skills.Combined
        CombinedSkillBreakdown.observe (patterns.Density50 * rate * combined_multiplier, accuracy, patterns.Duration / rate) skills.Combined
        CombinedSkillBreakdown.observe (patterns.Density75 * rate * combined_multiplier, accuracy, patterns.Duration / rate * 0.5f) skills.Combined
        CombinedSkillBreakdown.observe (patterns.Density90 * rate * combined_multiplier, accuracy, patterns.Duration / rate * 0.2f) skills.Combined

        for p in patterns.Clusters do

            let time =
                patterns.Clusters
                |> Seq.filter (fun p2 -> p2.Pattern = p.Pattern && p2.BPM >= p.BPM && p2.Density50 >= p.Density50)
                |> Seq.sumBy _.Amount

            let skill =
                match p.Pattern with
                | Jacks -> skills.Jacks
                | Chordstream -> skills.Chordstream
                | Stream -> skills.Stream

            PatternSkillBreakdown.observe p.Pattern (p.Density10 * rate, accuracy, time / rate * 1.8f) skill
            PatternSkillBreakdown.observe p.Pattern (p.Density25 * rate, accuracy, time / rate * 1.5f) skill
            PatternSkillBreakdown.observe p.Pattern (p.Density50 * rate, accuracy, time / rate) skill
            PatternSkillBreakdown.observe p.Pattern (p.Density75 * rate, accuracy, time / rate * 0.5f) skill
            PatternSkillBreakdown.observe p.Pattern (p.Density90 * rate, accuracy, time / rate * 0.2f) skill

    let score (patterns: PatternReport) (accuracy: float) (rate: Rate) (skills: KeymodeSkillBreakdown) : KeymodeSkillIncrease =

        let before = skills.Copy

        skill_increase patterns accuracy rate skills

        skills.Minus before

    let what_if (patterns: PatternReport) (accuracy: float) (rate: Rate) (skills: KeymodeSkillBreakdown) : KeymodeSkillIncrease =

        let potential = skills.Copy

        skill_increase patterns accuracy rate potential

        potential.Minus skills