namespace Interlude.Features.EditNoteskin

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Content.Noteskins
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Features.Gameplay

type EditHUDPage() as this =
    inherit Page()

    let open_hud_editor () =
        if
            Chart.WITH_COLORS.IsSome
            && Screen.change_new
                (fun () -> HUDEditor.edit_hud_screen (Chart.CHART.Value, Chart.WITH_COLORS.Value, fun () -> EditHUDPage().Show()))
                Screen.Type.Practice
                Transitions.Flags.Default
        then
            Menu.Exit()

    let hud_configuration (e: HUDElement) =
        let is_enabled = HUDElement.enabled_setting e

        let body =
            NavigationContainer.Row<Widget>(WrapNavigation = false)
            |+ if HUDElement.can_toggle e then
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
            |+ if HUDElement.can_configure e then 
                    [
                        Button(
                            Icons.SETTINGS + " " + %"hud.editor.options",
                            (fun () -> HUDElement.show_menu e ignore),
                            Position = Position.TrimLeft(PRETTYTEXTWIDTH + 100.0f).Margin(Style.PADDING)
                        )
                    ]
                else 
                    []
            |+ Text(
                HUDElement.name e,
                Align = Alignment.LEFT,
                Position = Position.SliceLeft(PRETTYTEXTWIDTH).Margin(Style.PADDING)
            )
            |+ Tooltip(
                Callout.Normal
                    .Title(HUDElement.name e)
                    .Body(HUDElement.tooltip e)
            )

        { new Container(NodeType.Container(K(Some body))) with
            override this.Draw() =
                if this.Focused then
                    Draw.rect this.Bounds Colors.yellow_accent.O1

                base.Draw()
        }
        |+ body

    let noteskin_required =
        Callout
            .Small
            .Title(%"hud.noteskin_required.title")
            .Body(%"hud.noteskin_required.general")
            .Button(%"hud.noteskin_required.button", fun () -> Menu.Back(); Shared.choose_noteskins())

    do
        NavigationContainer.Column<Widget>(Position = Position.Margin(100.0f, 200.0f))
        |+ (GridFlowContainer<Widget>(
                PRETTYHEIGHT,
                2,
                Spacing = (10.0f, 10.0f),
                Position = Position.TrimTop(PRETTYHEIGHT + 50.0f),
                WrapNavigation = false
            )
            |+ hud_configuration HUDElement.Accuracy
            |+ hud_configuration HUDElement.TimingDisplay
            |+ hud_configuration HUDElement.Combo
            |+ hud_configuration HUDElement.SkipButton
            |+ hud_configuration HUDElement.ProgressMeter
            |+ hud_configuration HUDElement.Pacemaker
            |+ hud_configuration HUDElement.JudgementCounter
            |+ hud_configuration HUDElement.JudgementMeter
            |+ hud_configuration HUDElement.EarlyLateMeter
            |+ hud_configuration HUDElement.RateModMeter
            |+ hud_configuration HUDElement.BPMMeter
        )
        |+ (if Interlude.Content.Content.Noteskin.IsEmbedded then
                Callout.frame noteskin_required (fun (w, h) -> Position.Box(0.0f, -0.15f, 0.0f, 0.0f, w, h)) :> Widget
            else
                PageButton("hud.editor", open_hud_editor, Position = Position.SliceTop(PRETTYHEIGHT))
        )
        |> this.Content

    override this.Title = %"hud.name"
    override this.OnClose() = ()
