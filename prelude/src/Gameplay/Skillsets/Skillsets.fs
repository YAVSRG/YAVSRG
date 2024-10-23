﻿namespace Prelude.Gameplay

open Percyqaz.Data
open Prelude
open Prelude.Charts.Processing.Patterns

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

module KeymodeSkillBreakdown =

    let private skill_increase (patterns: PatternReport) (accuracy: float) (rate: Rate) (skills: KeymodeSkillBreakdown) : unit =

        CombinedSkillBreakdown.observe (patterns.Density10 * rate, accuracy, patterns.Duration / rate * 1.8f) skills.Combined
        CombinedSkillBreakdown.observe (patterns.Density25 * rate, accuracy, patterns.Duration / rate * 1.5f) skills.Combined
        CombinedSkillBreakdown.observe (patterns.Density50 * rate, accuracy, patterns.Duration / rate) skills.Combined
        CombinedSkillBreakdown.observe (patterns.Density75 * rate, accuracy, patterns.Duration / rate * 0.5f) skills.Combined
        CombinedSkillBreakdown.observe (patterns.Density90 * rate, accuracy, patterns.Duration / rate * 0.2f) skills.Combined

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