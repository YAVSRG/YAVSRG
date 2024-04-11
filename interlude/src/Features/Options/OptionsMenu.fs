namespace Interlude.Features.OptionsMenu

open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Interlude.Content
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Features.LevelSelect
open Interlude.Features.OptionsMenu.Noteskins
open Interlude.Features.Gameplay

module OptionsMenuRoot =

    type private TileButton(body: Callout, onclick: unit -> unit) =
        inherit
            Container(
                NodeType.Button(fun () ->
                    Style.click.Play()
                    onclick ()
                )
            )

        let body_width, body_height = Callout.measure body

        member val Disabled = false with get, set
        member val Margin = (0.0f, 20.0f) with get, set

        override this.OnFocus(by_mouse: bool) =
            base.OnFocus by_mouse
            Style.hover.Play()

        override this.Init(parent) =
            this |* Clickable.Focus this
            base.Init(parent)

        override this.Draw() =
            let color =
                if this.Disabled then Colors.shadow_1
                elif this.Focused then Colors.pink_accent
                else Colors.shadow_1

            Draw.rect this.Bounds color.O3
            Draw.rect (this.Bounds.Expand(0.0f, 5.0f).SliceBottom(5.0f)) color

            Callout.draw (
                this.Bounds.Left + fst this.Margin,
                this.Bounds.Top + snd this.Margin,
                body_width,
                body_height,
                Colors.text,
                body
            )

    type OptionsPage() as this =
        inherit Page()

        let button_size =
            let _, h = Callout.measure (Callout.Normal.Title("Example"))
            h + 40.0f

        let tooltip_hint =
            Callout.Normal
                .Icon(Icons.INFO)
                .Title(%"options.ingame_help.name")
                .Body(%"options.ingame_help.hint")
                .Hotkey("tooltip")

        do
            let _, h = Callout.measure tooltip_hint

            GridFlowContainer<Widget>(
                button_size,
                3,
                Spacing = (50.0f, h + 120.0f),
                Position =
                    {
                        Left = 0.0f %+ 200.0f
                        Right = 1.0f %- 200.0f
                        Top = 0.5f %- (60.0f + h * 0.5f + button_size)
                        Bottom = 0.5f %+ (60.0f + h * 0.5f + button_size)
                    }
            )
            |+ TileButton(
                Callout.Normal.Icon(Icons.AIRPLAY).Title(%"system.name"),
                fun () -> System.SystemPage().Show()
            )
            |+ TileButton(
                Callout.Normal.Icon(Icons.SLIDERS).Title(%"gameplay.name"),
                fun () -> Gameplay.GameplayPage().Show()
            )
            |+ TileButton(
                Callout.Normal.Icon(Icons.IMAGE).Title(%"noteskins.name"),
                fun () -> NoteskinsPage().Show()
            )
                .Tooltip(Tooltip.Info("noteskins"))
            |+ TileButton(
                Callout.Normal.Icon(Icons.TERMINAL).Title(%"debug.name"),
                fun () -> Debug.DebugPage().Show()
            )
            |+ TileButton(
                Callout.Normal.Icon(Icons.HEART).Title(%"advanced.name"),
                fun () -> Advanced.AdvancedPage().Show()
            )
            |+ TileButton(Callout.Normal.Icon(Icons.ZAP).Title(%"hud.name"), 
                (fun () -> 
                    if Content.Noteskin.IsEmbedded then
                        EditHUDPage().Show()
                    elif
                        Chart.WITH_COLORS.IsSome
                        && Screen.change_new
                            (fun () -> HUDEditor.edit_hud_screen (Chart.CHART.Value, Chart.WITH_COLORS.Value, fun () -> OptionsPage().Show()))
                            Screen.Type.Practice
                            Transitions.Flags.Default
                    then
                        Menu.Exit()
                )
            )
                .Tooltip(Tooltip.Info("hud"))
            |> this.Content

            this
            |* Callout.frame
                (tooltip_hint)
                (fun (w, h) ->
                    {
                        Left = 0.0f %+ 200.0f
                        Right = 1.0f %- 200.0f
                        Top = 0.5f %- (h * 0.5f)
                        Bottom = 0.5f %+ (h * 0.5f)
                    }
                )

        override this.Title = %"options.name"
        override this.OnClose() = LevelSelect.refresh_all ()

    let show () = Menu.ShowPage OptionsPage
