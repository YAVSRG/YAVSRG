namespace Prelude.Data

open Prelude.Gameplay
open Prelude.Data.Library

module Skillsets =

    let keymode_skills = Array.init 8 (fun _ -> KeymodeSkillBreakdown.Default)

    let calculate (score_db: ScoreDatabase) (library: Library) =

        let sc_j4 = PremadeRulesets.SC.create 4
        let sc_j4_id = Ruleset.hash sc_j4

        for cc_key in library.Cache.Entries.Keys do
            let cc = library.Cache.Entries.[cc_key]
            match library.Cache.Patterns.TryGetValue cc.Hash with
            | false, _ -> ()
            | true, res ->

            let data = ScoreDatabase.get cc.Hash score_db
            match data.PersonalBests.TryFind(sc_j4_id) with
            | Some pbs ->
                for (acc, rate, _) in pbs.Accuracy do
                    KeymodeSkillBreakdown.score res.Patterns acc rate keymode_skills.[cc.Keys - 3] |> ignore
            | None -> ()

    let find_underperformance (score_db: ScoreDatabase) (library: Library) =

        let sc_j4 = PremadeRulesets.SC.create 4
        let sc_j4_id = Ruleset.hash sc_j4

        for cc_key in library.Cache.Entries.Keys do
            let cc = library.Cache.Entries.[cc_key]
            match library.Cache.Patterns.TryGetValue cc.Hash with
            | false, _ -> ()
            | true, res ->

            let data = ScoreDatabase.get cc.Hash score_db
            match data.PersonalBests.TryFind(sc_j4_id) with
            | Some pbs ->
                for (acc, rate, _) in pbs.Accuracy do
                    let double_quality_acc = 1.0 - 0.5 * (1.0 - acc)
                    let improvement = KeymodeSkillBreakdown.what_if res.Patterns double_quality_acc rate keymode_skills.[cc.Keys - 3]
                    if improvement.Total = 0.0f then
                        printfn "%.2f%% [%.2fx] on %s is an accuracy underperformance" (acc * 100.0) rate cc.Title
            | None -> ()