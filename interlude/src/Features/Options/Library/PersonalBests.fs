namespace Interlude.Features.OptionsMenu.Library

open Prelude
open Prelude.Data.Maintenance
open Prelude.Gameplay.Rulesets
open Interlude.Content
open Interlude.UI

module PersonalBests =

    let recalculate () =
        let request : PersonalBests.RecalculateRequest =
            {
                UserDatabase = Content.UserData
                ChartDatabase = Content.Charts
                Rulesets = Rulesets.list() |> Seq.map (fun (id, rs) -> Ruleset.hash rs, rs) |> Array.ofSeq
                MergeWithExisting = false
            }
            
        PersonalBests.recalculate_service.Request(
            request,
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