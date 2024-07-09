namespace Interlude.Features.Rulesets

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Interlude.Content
open Interlude.Options
open Interlude.UI

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
            container.Add(
                NavigationContainer.Row()
                |+ RulesetButton(
                    ruleset.Name,
                    (fun () -> options.SelectedRuleset.Set id),
                    Position = Position.TrimRight 100.0f
                )
                |+ Button(
                    Icons.TRASH,
                    (fun () -> 
                        ConfirmPage(
                            [ruleset.Name] %> "rulesets.confirm_delete",
                            fun () -> Rulesets.delete id |> ignore
                        )
                            .Show()
                    ),
                    Position = Position.SliceRight 100.0f
                )
            )

        if container.Focused then
            container.Focus false

    override this.Content() =
        refresh ()
        ScrollContainer(container, Position = Position.Margin(PRETTY_MARGIN_X, PRETTY_MARGIN_Y))

    override this.Title = sprintf "%s %s" Icons.SLIDERS (%"rulesets")
    override this.OnClose() = ()
    override this.OnReturnFromNestedPage() = refresh ()
