namespace Interlude.Features.Rulesets.Edit

open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay.Rulesets
open Interlude.UI

type EditJudgementPage(ruleset: Setting<Ruleset>, id: int) =
    inherit Page()

    let judgement = ruleset.Value.Judgements.[id]
    let name = Setting.simple judgement.Name
    let color = Setting.simple judgement.Color
    let breaks_combo = Setting.simple judgement.BreaksCombo

    override this.Content() =
        page_container()
        |+ PageTextEntry(%"rulesets.judgement.name", name)
            .Pos(0)
        |+ PageSetting(%"rulesets.judgement.color", ColorPicker(%"rulesets.judgement.color", color, false))
            .Pos(2)
        |+ PageSetting(%"rulesets.judgement.breaks_combo", Checkbox breaks_combo)
            .Pos(4)
        :> Widget

    override this.Title = judgement.Name
    override this.OnClose() =
        let new_js = ruleset.Value.Judgements |> Array.copy
        new_js.[id] <- { judgement with Name = name.Value.Trim(); Color = color.Value; BreaksCombo = breaks_combo.Value }
        ruleset.Set { ruleset.Value with Judgements = new_js }

type EditJudgementsPage(ruleset: Setting<Ruleset>) =
    inherit Page()

    let container = FlowContainer.Vertical<Widget>(PAGE_ITEM_HEIGHT)

    let rec judgement_controls (i: int, j: Judgement) =
        NavigationContainer.Row()
        |+ PageButton(j.Name, fun () -> EditJudgementPage(ruleset, i).Show())
            .TextColor(j.Color)
            .Position(Position.ShrinkR(PAGE_ITEM_HEIGHT * 2.0f))
        |+ Button(Icons.COPY, fun () ->
            ConfirmPage(
                [j.Name] %> "rulesets.judgement.confirm_duplicate",
                fun () -> duplicate_judgement i
            ).Show()
        )
            .Position(Position.SliceR(PAGE_ITEM_HEIGHT).TranslateX(-PAGE_ITEM_HEIGHT))
        |+ Button(Icons.TRASH, fun () ->
            ConfirmPage(
                [j.Name] %> "rulesets.judgement.confirm_delete",
                fun () -> delete_judgement i
            ).Show()
        )
            .Disabled(fun () -> ruleset.Value.Judgements.Length <= 1)
            .Position(Position.SliceR(PAGE_ITEM_HEIGHT))

    and refresh() : unit =
        container.Clear()
        for i, j in ruleset.Value.Judgements |> Array.indexed do
            container.Add (judgement_controls (i, j))

    and duplicate_judgement(i: int) : unit =
        ruleset.Set (Ruleset.duplicate_judgement i ruleset.Value)
        GameThread.defer refresh

    and delete_judgement (i: int) : unit =
        ruleset.Set (Ruleset.remove_judgement i ruleset.Value)
        refresh()

    override this.Content() =
        refresh()
        ScrollContainer(container)
            .Position(Position.Shrink(PAGE_MARGIN_X, PAGE_MARGIN_Y).SliceL(PAGE_ITEM_WIDTH))

    override this.Title = %"rulesets.edit.judgements"
    override this.OnClose() = ()
    override this.OnReturnFromNestedPage() = refresh()