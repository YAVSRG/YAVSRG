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
            validation_message.Current <-
            match Ruleset.check rs with
            | Ok _ -> Dummy() :> Widget
            | Error reason ->
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
                    .Position(fun (w, h) -> Position.Box(0.0f, 0.0f, 0.0f, 0.0f, w, h))
        )

    let name = Setting.simple ruleset.Value.Name

    member this.SaveChangesIfValid() =
        let has_error = match validation_message.Current with :? Dummy -> false | _ -> true
        if not has_error then
            Rulesets.update id { ruleset.Value with Name = name.Value.Trim() }

    override this.Content() =
        this.OnClose(this.SaveChangesIfValid)

        page_container()
            .With(
                PageTextEntry(%"rulesets.edit.name", name)
                    .Pos(0),
                PageButton(%"rulesets.edit.judgements", fun () -> EditJudgementsPage(ruleset).Show())
                    .Pos(3),
                PageButton(%"rulesets.edit.windows", fun () -> EditWindowsPage.NoteWindowsPage(ruleset).Show())
                    .Pos(5),
                PageButton(%"rulesets.edit.accuracy", fun () -> ConfigureAccuracyPage(ruleset).Show())
                    .Pos(7),
                PageButton(%"rulesets.edit.mechanics", fun () -> EditMechanicsPage(ruleset).Show())
                    .Pos(9),
                PageButton(%"rulesets.edit.grades", fun () -> EditGradesPage(ruleset).Show())
                    .Pos(12),
                PageButton(%"rulesets.edit.lamps", fun () -> EditLampsPage(ruleset).Show())
                    .Pos(14),
                validation_message.Pos(17, PAGE_BOTTOM - 17)
            )

    override this.Title = original.Name