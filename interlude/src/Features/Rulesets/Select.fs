namespace Interlude.Features.Rulesets

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Interlude.Content
open Interlude.Options
open Interlude.UI
open Interlude.UI.Menu

type private RulesetButton(name, action) =
    inherit
        Container(
            NodeType.Button(fun _ ->
                Style.click.Play()
                action ()
            )
        )

    override this.Init(parent: Widget) =
        this
        |+ Text(
            K(sprintf "%s  >" name),
            Color =
                (fun () ->
                    if this.Focused then Colors.text_yellow_2 
                    elif Rulesets.current.Name = name then Colors.text_pink_2 
                    else Colors.text
                ),
            Align = Alignment.LEFT,
            Position = Position.Margin Style.PADDING
        )
        |* Clickable.Focus this

        base.Init parent

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Draw() =
        if this.Focused then
            Draw.rect this.Bounds Colors.yellow_accent.O1

        base.Draw()

type SelectRulesetPage() =
    inherit Page()

    let container = FlowContainer.Vertical<Widget>(PRETTYHEIGHT)

    let rec refresh () =
        container.Clear()

        container
        |+ PageButton(
            %"rulesets.add",
            (fun () -> AddRulesetsPage().Show()),
            Icon = Icons.DOWNLOAD
        )
        |* Dummy()

        for id, ruleset in Rulesets.list () do
            container
            |* RulesetButton(
                ruleset.Name,
                fun () -> options.SelectedRuleset.Set id
            )

        if container.Focused then
            container.Focus false

    override this.Content() =
        refresh ()
        ScrollContainer(container, Position = Position.Margin(100.0f, 200.0f))

    override this.Title = sprintf "%s %s" Icons.SLIDERS (%"rulesets")
    override this.OnClose() = ()
    override this.OnReturnFromNestedPage() = refresh ()
