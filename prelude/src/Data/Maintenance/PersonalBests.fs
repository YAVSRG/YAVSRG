namespace Prelude.Data.Maintenance

open Percyqaz.Common
open Prelude.Mods
open Prelude.Gameplay.Rulesets
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

    let recalculate_service =
        { new Async.Service<RecalculateRequest, unit>() with
            override this.Handle(request) =
                async {
                    for cc in request.ChartDatabase.Entries |> Seq.toArray do
                        let data = UserDatabase.get_chart_data cc.Hash request.UserDatabase

                        if not data.Scores.IsEmpty then
                            match ChartDatabase.get_chart cc.Hash request.ChartDatabase with
                            | Error reason ->
                                Logging.Debug "Couldn't load %s for pb processing: %s" cc.Hash reason
                            | Ok chart ->

                            let existing_bests = data.PersonalBests
                            let mutable new_bests = if request.MergeWithExisting then data.PersonalBests else Map.empty

                            for score in data.Scores do
                                let _, initial_ruleset = request.Rulesets.[0]
                                let score_info = ScoreInfo.from_score cc chart initial_ruleset score

                                if score_info.ModStatus = ModStatus.Ranked then

                                    for ruleset_id, ruleset in request.Rulesets do
                                        score_info.Ruleset <- ruleset

                                        if new_bests.ContainsKey ruleset_id then
                                            let ruleset_bests, _ = Bests.update score_info new_bests.[ruleset_id]
                                            new_bests <- Map.add ruleset_id ruleset_bests new_bests
                                        else
                                            new_bests <- Map.add ruleset_id (Bests.create score_info) new_bests

                            if new_bests <> existing_bests then
                                data.PersonalBests <- new_bests

                    UserDatabase.save_changes request.UserDatabase
                    Logging.Info("Finished processing personal bests for all rulesets")
                }
        }