namespace Interlude.Features.Rulesets.Edit

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay
open Interlude.UI

type EditJudgementPage(ruleset: Setting<Ruleset>, id: int) =
    inherit Page()

    let judgement = ruleset.Value.Judgements.[id]
    let name = Setting.simple judgement.Name
    let color = Setting.simple judgement.Color

    override this.Content() =
        page_container()
        |+ PageTextEntry(%"rulesets.judgement.name", name)
            .Pos(0)
        |+ PageSetting(%"rulesets.judgement.color", ColorPicker(color, false))
            .Pos(2, 3)
        // todo: checkbox if it breaks combo
        // todo: maybe put timing windows here as it makes most logical sense
        :> Widget

    override this.Title = judgement.Name
    override this.OnClose() =
        let new_js = ruleset.Value.Judgements |> Array.copy
        new_js.[id] <- { judgement with Name = name.Value.Trim(); Color = color.Value }
        ruleset.Set { ruleset.Value with Judgements = new_js }

type EditJudgementsPage(ruleset: Setting<Ruleset>) =
    inherit Page()

    let judgement_controls (i: int, j: Judgement) =
        NavigationContainer.Row()
        |+ ColoredButton(j.Name, j.Color, (fun () -> EditJudgementPage(ruleset, i).Show()), Position = Position.ShrinkR PRETTYHEIGHT)
        |+ Button(Icons.TRASH, ignore, Position = Position.SliceR PRETTYHEIGHT)

    let container = FlowContainer.Vertical<Widget>(PRETTYHEIGHT)

    let refresh() =
        container.Clear()
        for i, j in ruleset.Value.Judgements |> Array.indexed do
            container.Add (judgement_controls (i, j))

    override this.Content() =
        refresh()
        ScrollContainer(container, Position = Position.Shrink(PRETTY_MARGIN_X, PRETTY_MARGIN_Y).SliceL(PRETTYWIDTH))

    override this.Title = %"rulesets.edit.judgements"
    override this.OnClose() = ()
    override this.OnReturnFromNestedPage() = refresh()
