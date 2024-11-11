namespace Prelude.Data.User

open Prelude
open Prelude.Gameplay
open Prelude.Gameplay.Rulesets
open Prelude.Data.Library

module Skillsets =

    let keymode_skills = Array.init 8 (fun _ -> KeymodeSkillBreakdown.Default)

    let calculate (score_db: UserDatabase) (library: Library) =

        let sc_j4 = SC.create 4
        let sc_j4_id = Ruleset.hash sc_j4

        for cc_key in library.Charts.Cache.Keys do
            let cc = library.Charts.Cache.[cc_key]

            let data = UserDatabase.get_chart_data cc.Hash score_db
            match data.PersonalBests.TryFind(sc_j4_id) with
            | Some pbs ->
                for (acc, rate, _) in pbs.Accuracy do
                    KeymodeSkillBreakdown.score cc.Patterns acc rate keymode_skills.[cc.Keys - 3] |> ignore
            | None -> ()