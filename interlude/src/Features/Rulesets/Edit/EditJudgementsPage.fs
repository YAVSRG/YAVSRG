namespace Interlude.Features.Rulesets.Edit

open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay.Rulesets
open Interlude.UI

type EditJudgementsPage(ruleset: Setting<Ruleset>) =
    inherit Page()

    let container = FlowContainer.Vertical<Widget>(PAGE_ITEM_HEIGHT)

    let rec judgement_controls (i: int, j: Judgement) =
        NavigationContainer
            .Row()
            .With(
                PageButton(j.Name, fun () -> EditJudgementPage(ruleset, i).Show())
                    .TextColor(j.Color)
                    .Position(Position.ShrinkR(PAGE_ITEM_HEIGHT * 2.0f)),
                Button(
                    Icons.COPY,
                    fun () ->
                        ConfirmPage(
                            [ j.Name ] %> "rulesets.judgement.confirm_duplicate",
                            fun () -> duplicate_judgement i
                        )
                            .Show()
                )
                    .Position(Position.SliceR(PAGE_ITEM_HEIGHT, PAGE_ITEM_HEIGHT)),
                Button(
                    Icons.TRASH,
                    fun () ->
                        ConfirmPage([ j.Name ] %> "rulesets.judgement.confirm_delete", fun () -> delete_judgement i)
                            .Show()
                )
                    .Disabled(fun () -> ruleset.Value.Judgements.Length <= 1)
                    .TextColor(Colors.red_accent)
                    .Position(Position.SliceR(PAGE_ITEM_HEIGHT))
            )

    and refresh () : unit =
        container.Clear()

        for i, j in ruleset.Value.Judgements |> Array.indexed do
            container.Add(judgement_controls(i, j))

    and duplicate_judgement (i: int) : unit =
        ruleset.Set(ruleset.Value.WithDuplicatedJudgement(i))
        GameThread.defer refresh

    and delete_judgement (i: int) : unit =
        ruleset.Set(ruleset.Value.WithoutJudgement(i))
        refresh()

    override this.Content() : Widget =
        refresh()
        ScrollContainer(container).Position(Position.Shrink(PAGE_MARGIN_X, PAGE_MARGIN_Y).SliceL(PAGE_ITEM_WIDTH))

    override this.Title = %"rulesets.edit.judgements"
    override this.OnReturnFromNestedPage() : unit = refresh()
