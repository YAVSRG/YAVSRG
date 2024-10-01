namespace Interlude.Features.Rulesets.Edit

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay.Rulesets
open Interlude.UI
open Interlude.Content

type RulesetEditorPage(id: string, original: Ruleset) =
    inherit Page()

    let ruleset = Setting.simple original

    let name = Setting.simple ruleset.Value.Name

    override this.Content() =
        page_container()
        |+ PageTextEntry(%"rulesets.edit.name", name)
            .Pos(0)
        |+ PageButton(%"rulesets.edit.judgements", fun () -> EditJudgementsPage(ruleset).Show())
            .Pos(3)
        |+ PageButton(%"rulesets.edit.grades", fun () -> EditGradesPage(ruleset).Show())
            .Pos(5)
        |+ PageButton(%"rulesets.edit.lamps", fun () -> EditLampsPage(ruleset).Show())
            .Pos(7)
        :> Widget

    override this.Title = original.Name
    override this.OnClose() =
        if System.Object.ReferenceEquals(original, ruleset.Value) |> not || name.Value <> original.Name then
            Rulesets.install_or_update id { ruleset.Value with Name = name.Value.Trim() }