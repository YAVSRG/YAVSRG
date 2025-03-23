namespace Interlude.Features.Rulesets

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Interlude.Content
open Interlude.Options
open Interlude.UI
open Interlude.Features.Rulesets.Edit

type private RulesetButton(id: string, name: string, action: unit -> unit) =
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
                    elif Rulesets.selected_id.Value = id then Colors.text_pink_2
                    else Colors.text
                ),
            Align = Alignment.LEFT,
            Position = Position.Shrink Style.PADDING
        )
        |* Clickable.Focus this

        base.Init parent

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Draw() =
        if this.Focused then
            Render.rect this.Bounds Colors.yellow_accent.O1

        base.Draw()

type SelectRulesetPage() =
    inherit Page()

    let container = FlowContainer.Vertical<Widget>(PAGE_ITEM_HEIGHT)

    let refresh () =
        container.Clear()

        container
        |+ PageButton(
            %"rulesets.add",
            (fun () -> AddRulesetsPage().Show()),
            Icon = Icons.DOWNLOAD
        )
        |+ PageButton(
            %"rulesets.open_folder",
            (fun () -> open_directory (get_game_folder "Rulesets")),
            Icon = Icons.FOLDER
        )
        |* Dummy()

        for id, ruleset in Rulesets.list () do
            container.Add(
                NavigationContainer.Row()
                |+ RulesetButton(
                    id,
                    ruleset.Name,
                    (fun () -> options.SelectedRuleset.Set id),
                    Position = Position.ShrinkR(PAGE_ITEM_HEIGHT * 3.0f)
                )
                |+ Button(
                    Icons.EDIT,
                    (fun () ->
                        RulesetEditorPage(id, ruleset).Show()
                    ),
                    Position = Position.SliceR(PAGE_ITEM_HEIGHT).TranslateX(-PAGE_ITEM_HEIGHT * 2.0f)
                )
                |+ Button(
                    Icons.COPY,
                    (fun () ->
                        ConfirmPage(
                            [ruleset.Name] %> "rulesets.confirm_copy",
                            fun () ->
                                Rulesets.install { ruleset.Clone with Name = ruleset.Name + " (Copy)" }
                        )
                            .Show()
                    ),
                    Position = Position.SliceR(PAGE_ITEM_HEIGHT).TranslateX(-PAGE_ITEM_HEIGHT)
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
                    Position = Position.SliceR PAGE_ITEM_HEIGHT,
                    Disabled = K (id = Rulesets.DEFAULT_ID)
                )
            )

        if container.Focused then
            container.Focus false

    override this.Content() =
        refresh ()
        ScrollContainer(container, Position = Position.Shrink(PAGE_MARGIN_X, PAGE_MARGIN_Y).SliceL(PAGE_ITEM_WIDTH))

    override this.Title = sprintf "%s %s" Icons.SLIDERS (%"rulesets")
    override this.OnClose() = ()
    override this.OnReturnFromNestedPage() = refresh ()