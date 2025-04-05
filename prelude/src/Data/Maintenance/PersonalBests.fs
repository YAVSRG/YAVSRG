namespace Prelude.Data.Maintenance

open Percyqaz.Common
open Prelude.Mods
open Prelude.Gameplay.Rulesets
open Prelude.Data
open Prelude.Data.User
open Prelude.Data.Library

module PersonalBests =

    type RecalculateRequest =
        {
            ChartDatabase: ChartDatabase
            UserDatabase: UserDatabase
            Rulesets: (string * Ruleset) array
            MergeWithExisting: bool
        }

    let recalculate (rulesets: (string * Ruleset) array, merge_with_existing: bool, chart_db: ChartDatabase, user_db: UserDatabase, progress: ProgressCallback) =
        if rulesets.Length = 0 then
            async { progress Complete }
        else

        async {
            let charts = chart_db.Entries |> Seq.toArray
            for i, chart_meta in Seq.indexed charts do
                let data = UserDatabase.get_chart_data chart_meta.Hash user_db

                if not data.Scores.IsEmpty then
                    match ChartDatabase.get_chart chart_meta.Hash chart_db with
                    | Error reason ->
                        Logging.Debug "Couldn't load '%s' for pb processing: %s" chart_meta.Hash reason
                    | Ok chart ->

                    let existing_bests = data.PersonalBests
                    let mutable new_bests = if merge_with_existing then data.PersonalBests else Map.empty

                    for score in data.Scores do
                        let _, initial_ruleset = rulesets.[0]
                        let score_info = ScoreInfo.from_score chart_meta chart initial_ruleset score

                        if score_info.ModStatus = ModStatus.Ranked then

                            for ruleset_id, ruleset in rulesets do
                                score_info.Ruleset <- ruleset

                                if new_bests.ContainsKey ruleset_id then
                                    let ruleset_bests, _ = Bests.update score_info new_bests.[ruleset_id]
                                    new_bests <- Map.add ruleset_id ruleset_bests new_bests
                                else
                                    new_bests <- Map.add ruleset_id (Bests.create score_info) new_bests

                    if new_bests <> existing_bests then
                        data.PersonalBests <- new_bests
                progress (Processing (i + 1, charts.Length))

            UserDatabase.save_changes user_db
            progress Complete
        }