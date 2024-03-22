﻿namespace Interlude.Features.OptionsMenu.Noteskins

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude.Content.Noteskins
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Features.Gameplay

type EditHUDPage() as this =
    inherit Page()

    let open_hud_editor () =
        if
            Chart.WITH_COLORS.IsSome
            && Screen.change_new
                (fun () -> HUDEditor.edit_hud_screen (Chart.CHART.Value, Chart.WITH_COLORS.Value))
                Screen.Type.Practice
                Transitions.Flags.Default
        then
            Menu.Exit()

    let hud_configuration (e: HUDElement) =
        let is_enabled = HUDElement.enabled_setting e

        let body =
            NavigationContainer.Row<Widget>(WrapNavigation = false)
            |+ if e <> HUDElement.Pacemaker then
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
            |+ Button(
                Icons.SETTINGS + " " + "Options",
                (fun () -> HUDElement.show_menu e ignore),
                Position = Position.TrimLeft(PRETTYTEXTWIDTH + 100.0f).Margin(Style.PADDING)
            )
            |+ Text(
                HUDElement.name e,
                Align = Alignment.LEFT,
                Position = Position.SliceLeft(PRETTYTEXTWIDTH).Margin(Style.PADDING)
            )
            |+ Tooltip(Tooltip.Info(HUDElement.tooltip e))

        { new StaticContainer(NodeType.Container(K(Some body))) with
            override this.Draw() =
                if this.Focused then
                    Draw.rect this.Bounds Colors.yellow_accent.O1

                base.Draw()
        }
        |+ body

    do
        this.Content(
            NavigationContainer.Column<Widget>(Position = Position.Margin(100.0f, 200.0f))
            |+ PageButton("hud.edit", open_hud_editor, Position = Position.SliceTop(PRETTYHEIGHT))
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
                |+ hud_configuration HUDElement.BPMMeter)
        )

    override this.Title = %"hud.name"
    override this.OnClose() = ()
