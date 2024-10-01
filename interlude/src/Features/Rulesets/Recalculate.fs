namespace Interlude.Features.Rulesets

open Percyqaz.Common
open Prelude
open Prelude.Gameplay.Mods
open Prelude.Gameplay.Rulesets
open Prelude.Data.User
open Prelude.Data.Library
open Interlude.UI
open Interlude.Content

module PersonalBests =

    // todo: move to prelude
    let private personal_best_fixer =
        { new Async.Service<(string * Ruleset) array, unit>() with
            override this.Handle(rulesets) =
                async {
                    for cc in Content.Charts.Entries |> Seq.toArray do
                        let data = UserDatabase.get_chart_data cc.Hash Content.UserData

                        if not data.Scores.IsEmpty then
                            match ChartDatabase.get_chart cc.Hash Content.Charts with
                            | Error reason ->
                                Logging.Debug(sprintf "Couldn't load %s for pb processing: %s" cc.Hash reason)
                            | Ok chart ->

                            let existing_bests = data.PersonalBests
                            let mutable new_bests = existing_bests

                            for score in data.Scores do
                                let _, initial_ruleset = rulesets.[0]
                                let score_info = ScoreInfo.from_score cc chart initial_ruleset score

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

                    Logging.Info(sprintf "Finished processing personal bests for all rulesets")
                }
        }

    let recalculate () =
        personal_best_fixer.Request(
            Rulesets.list() |> Seq.map (fun (id, rs) -> Ruleset.hash rs, rs) |> Array.ofSeq,
            fun () ->
                Notifications.system_feedback (
                    Icons.ALERT_OCTAGON,
                    %"notification.score_recalculation_complete.title",
                    ""
                )
        )

        Notifications.system_feedback (
            Icons.ALERT_OCTAGON,
            %"notification.score_recalculation_started.title",
            %"notification.score_recalculation_started.body"
        )