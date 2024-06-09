namespace Interlude.Features.OptionsMenu

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Interlude.Content
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Features.LevelSelect
open Interlude.Features.EditNoteskin
open Interlude.Features.OptionsMenu.Search
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
                this.Bounds.Left,
                this.Bounds.Top,
                body_width,
                body_height,
                Colors.text,
                body
            )

    type OptionsPage() =
        inherit Page()

        let button_size = Callout.measure (Callout.Normal.Title("Example")) |> snd

        let tooltip_hint =
            Callout.Normal
                .Icon(Icons.INFO)
                .Title(%"options.ingame_help")
                .Body(%"options.ingame_help.hint")
                .Hotkey("tooltip")

        override this.Content() =
            let menu_buttons =
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
                    Callout.Normal.Icon(Icons.AIRPLAY).Title(%"system"),
                    fun () -> SystemSettings.SystemPage().Show()
                )
                |+ TileButton(
                    Callout.Normal.Icon(Icons.SLIDERS).Title(%"gameplay"),
                    fun () -> Gameplay.GameplayPage().Show()
                )
                |+ TileButton(
                    Callout.Normal.Icon(Icons.IMAGE).Title(%"noteskins"),
                    fun () -> Noteskins.NoteskinsPage().Show()
                )
                    .Tooltip(Tooltip.Info("noteskins"))
                |+ TileButton(
                    Callout.Normal.Icon(Icons.TERMINAL).Title(%"debug"),
                    fun () -> Debug.DebugPage().Show()
                )
                |+ TileButton(
                    Callout.Normal.Icon(Icons.HEART).Title(%"advanced"),
                    fun () -> Advanced.AdvancedPage().Show()
                )
                |+ TileButton(Callout.Normal.Icon(Icons.ZAP).Title(%"hud"), 
                    (fun () -> 
                        if Content.Noteskin.IsEmbedded then
                            EditHUDPage().Show()
                        elif
                            SelectedChart.WITH_COLORS.IsSome
                            && Screen.change_new
                                (fun () -> HUDEditor.edit_hud_screen (SelectedChart.CHART.Value, SelectedChart.WITH_COLORS.Value, fun () -> OptionsPage().Show()))
                                Screen.Type.Practice
                                Transitions.Default
                        then
                            Menu.Exit()
                    )
                )
                    .Tooltip(Tooltip.Info("hud"))
                |>> Container
                |+ Callout.frame
                    tooltip_hint
                    (fun (w, h) ->
                        {
                            Left = 0.0f %+ 200.0f
                            Right = 1.0f %- 200.0f
                            Top = 0.5f %- (h * 0.5f)
                            Bottom = 0.5f %+ (h * 0.5f)
                        }
                    )
            let search_text = Setting.simple ""

            let search_results = SwapContainer(menu_buttons)

            NavigationContainer.Column<Widget>()
            |+ search_results
            |+ { new SearchBox(
                    search_text, 
                    fun () ->
                        if search_text.Value = "" then
                            search_results.Current <- menu_buttons
                        else
                            search_results.Current <- SearchResults.get search_text.Value
                    , Position = Position.Margin(PRETTY_MARGIN_X * 2.0f, 25.0f).SliceTop(60.0f)
                    ) with
                override this.OnFocus by_mouse =
                    base.OnFocus by_mouse
                    if not by_mouse then defer (fun () -> this.Select false)
            }
            :> Widget

        override this.Title = %"options"
        override this.OnClose() = LevelSelect.refresh_all ()

    let show () = Menu.ShowPage OptionsPage
