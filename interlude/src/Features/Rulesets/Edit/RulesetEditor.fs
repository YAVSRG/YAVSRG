namespace Interlude.Features.Rulesets.Edit

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay.Rulesets
open Interlude.UI
open Interlude.Content

type RulesetEditorPage(id: string, original: Ruleset) =
    inherit Page()

    let validation_message = SwapContainer()

    let ruleset =
        Setting.simple original
        |> Setting.trigger (fun rs ->
            match Ruleset.check rs with
            | Ok _ -> validation_message.Current <- Dummy()
            | Error reason ->
                let card =
                    CalloutCard(
                        (Callout
                            .Normal
                            .Icon(Icons.ALERT_OCTAGON)
                            .Title(%"rulesets.edit.validation_error.title")
                            .Body(reason)
                            .Body(%"rulesets.edit.validation_error.body")
                        ),
                        Colors.red_accent,
                        Colors.red.O3
                    )
                card.Position <- Position.Box(0.0f, 0.0f, 0.0f, 0.0f, (card :> IWidth).Width, (card :> IHeight).Height)
                validation_message.Current <- card
        )

    let name = Setting.simple ruleset.Value.Name

    override this.Content() =
        page_container()
        |+ PageTextEntry(%"rulesets.edit.name", name)
            .Pos(0)
        |+ PageButton(%"rulesets.edit.judgements", fun () -> EditJudgementsPage(ruleset).Show())
            .Pos(3)
        |+ PageButton(%"rulesets.edit.windows", fun () -> EditWindows.note_windows(ruleset).Show())
            .Pos(5)
        |+ PageButton(%"rulesets.edit.accuracy", fun () -> ConfigureAccuracyPage(ruleset).Show())
            .Pos(7)
        |+ PageButton(%"rulesets.edit.mechanics", fun () -> EditMechanicsPage(ruleset).Show())
            .Pos(9)
        |+ PageButton(%"rulesets.edit.grades", fun () -> EditGradesPage(ruleset).Show())
            .Pos(12)
        |+ PageButton(%"rulesets.edit.lamps", fun () -> EditLampsPage(ruleset).Show())
            .Pos(14)
        |+ validation_message.Pos(17, PAGE_BOTTOM - 17)
        :> Widget

    override this.Title = original.Name
    override this.OnClose() =
        let has_error = match validation_message.Current with :? Dummy -> false | _ -> true
        if not has_error then
            Rulesets.update id { ruleset.Value with Name = name.Value.Trim() }