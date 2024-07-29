namespace Prelude.Data

open Prelude
open Prelude.Gameplay
open Prelude.Data.Library
open Prelude.Charts.Processing.Patterns

module Skillsets =

    let skills = KeymodeSkillBreakdown.Default

    let score (patterns: PatternSummary.PatternBreakdown list) (accuracy: float) (rate: float32) =
        for p in patterns do
            let time = 
                patterns 
                |> Seq.filter (fun p2 -> p2.Pattern = p.Pattern && p2.BPM >= p.BPM && p2.Density50 >= p.Density50)
                |> Seq.sumBy _.Amount

            KeymodeSkillBreakdown.observe p.Pattern (p.Density50 * rate, accuracy, Time.of_number (time / rate)) skills
            KeymodeSkillBreakdown.observe p.Pattern (p.Density75 * rate, accuracy, Time.of_number (time / rate * 0.5f)) skills
            KeymodeSkillBreakdown.observe p.Pattern (p.Density25 * rate, accuracy, Time.of_number (time / rate * 1.5f)) skills

    let calculate (score_db: ScoreDatabase) (library: Library) =

        let sc_j4 = PremadeRulesets.SC.create 4
        let sc_j4_id = Ruleset.hash sc_j4

        for cc_key in library.Cache.Entries.Keys do
            let cc = library.Cache.Entries.[cc_key]
            let data = ScoreDatabase.get cc.Hash score_db
            match data.PersonalBests.TryFind(sc_j4_id) with
            | Some pbs ->
                for (acc, rate, _) in pbs.Accuracy do
                    match library.Cache.Patterns.TryGetValue cc.Hash with
                    | true, res ->
                        score res.Patterns acc rate
                    | false, _ -> ()
            | None -> ()