namespace Interlude.Features.Noteskins.Edit
open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Skinning.HudLayouts
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Features.Gameplay

type EditHUDPage() =
    inherit Page()

    // todo: remove this as it can no longer be seen ?
    let open_hud_editor () =
        if
            SelectedChart.WITH_COLORS.IsSome
            && Screen.change_new
                (fun () -> HUDEditor.edit_hud_screen (SelectedChart.CHART.Value, SelectedChart.WITH_COLORS.Value, fun () -> EditHUDPage().Show()))
                Screen.Type.Practice
                Transitions.Default
        then
            Menu.Exit()

    let hud_configuration (elem: HudElement) =
        let is_enabled = HudElement.enabled_setting elem

        let body =
            NavigationContainer.Row(WrapNavigation = false)
            |+ if HudElement.can_toggle elem then
                   [
                       Button(
                           (fun () ->
                               if is_enabled.Value then
                                   Icons.CHECK_CIRCLE
                               else
                                   Icons.CIRCLE
                           ),
                           (fun () -> is_enabled.Value <- not is_enabled.Value),
                           Position = Position.Column(PRETTYTEXTWIDTH, 100.0f).Margin(Style.PADDING)
                       )
                   ]
               else
                   []
            |+ if HudElement.can_configure elem then 
                    [
                        Button(
                            Icons.SETTINGS + " " + %"hud.editor.options",
                            (fun () -> HudElement.show_menu elem ignore),
                            Position = Position.TrimLeft(PRETTYTEXTWIDTH + 100.0f).Margin(Style.PADDING)
                        )
                    ]
                else 
                    []
            |+ Text(
                HudElement.name elem,
                Align = Alignment.LEFT,
                Position = Position.SliceLeft(PRETTYTEXTWIDTH).Margin(Style.PADDING)
            )
            |+ Tooltip(
                Callout.Normal
                    .Title(HudElement.name elem)
                    .Body(HudElement.tooltip elem)
            )

        { new Container(NodeType.Container(K(Some body))) with
            override this.Draw() =
                if this.Focused then
                    Draw.rect this.Bounds Colors.yellow_accent.O1

                base.Draw()
        }
        |+ body

    // todo: untangle this when HUD is separate from noteskins
    let noteskin_required =
        Callout
            .Small
            .Title(%"hud.noteskin_required.title")
            .Body(%"hud.noteskin_required.general")
            .Button(%"hud.noteskin_required.button", fun () -> Menu.Back(); Shared.choose_noteskins())

    override this.Content() =
        NavigationContainer.Column(Position = Position.Margin(100.0f, 200.0f))
        |+ (GridFlowContainer<Widget>(
                PRETTYHEIGHT,
                2,
                Spacing = (10.0f, 10.0f),
                Position = Position.TrimTop(PRETTYHEIGHT + 50.0f),
                WrapNavigation = false
            )
            |+ seq { for elem in HudElement.FULL_LIST do yield hud_configuration elem }
        )
        |+ (if Interlude.Content.Content.Noteskin.IsEmbedded then
                Callout.frame noteskin_required (fun (w, h) -> Position.Box(0.0f, -0.15f, 0.0f, 0.0f, w, h)) :> Widget
            else
                PageButton(%"hud.editor", open_hud_editor, Position = Position.SliceTop(PRETTYHEIGHT))
        )
        :> Widget

    override this.Title = %"hud"
    override this.OnClose() = ()
