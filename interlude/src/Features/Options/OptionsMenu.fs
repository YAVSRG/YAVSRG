namespace Interlude.Features.OptionsMenu

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Interlude.Content
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Features.EditNoteskin
open Interlude.Features.OptionsMenu.Search
open Interlude.Features.Gameplay

module OptionsMenuRoot =

    type private TileButton(label: string, onclick: unit -> unit) =
        inherit
            Container(
                NodeType.Button(fun () ->
                    Style.click.Play()
                    onclick ()
                )
            )

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
            Draw.rect (this.Bounds.BorderBottom Style.PADDING) color

            Text.fill_b (Style.font, label, this.Bounds.SliceTop(60.0f).Shrink(Style.PADDING * 2.0f), Colors.text, Alignment.CENTER)

    type OptionsHeader(search_callback: string -> unit) as this =
        inherit Container(NodeType.Container(fun () -> Some this.Buttons))

        let HEIGHT = 100.0f

        let search_text = Setting.simple ""

        let search_box =
            { new SearchBox(
                    search_text, 
                    (fun () -> search_callback search_text.Value),
                    Position = Position.CenterY(60.0f).Margin(PRETTY_MARGIN_X, 0.0f).SliceRight(600.0f)) with
                override this.OnFocus by_mouse =
                    base.OnFocus by_mouse
                    if not by_mouse then defer (fun () -> this.Select false)
            }

        let tab_buttons =
            FlowContainer.LeftToRight(200.0f, Spacing = 5.0f, Position = Position.SliceBottom(60.0f).Margin(PRETTY_MARGIN_X, 0.0f))
            |+ TileButton(
                sprintf "%s %s" Icons.AIRPLAY (%"system"),
                fun () -> SystemSettings.SystemPage().Show()
            )
            |+ TileButton(
                sprintf "%s %s" Icons.SLIDERS (%"gameplay"),
                fun () -> Gameplay.GameplayPage().Show()
            )
            |+ TileButton(
                sprintf "%s %s" Icons.ARCHIVE (%"library"),
                fun () -> Library.LibraryPage().Show()
            )
            |+ TileButton(
                sprintf "%s %s" Icons.IMAGE (%"noteskins"),
                fun () -> Noteskins.NoteskinsPage().Show()
            )

        member private this.Buttons = tab_buttons
        
        override this.Init(parent) =
            this.Position <- Position.SliceTop(HEIGHT)
            this
            |+ search_box
            |* tab_buttons
            base.Init parent

        override this.Draw() =
            Draw.rect this.Bounds Colors.shadow_2.O2
            Draw.rect (this.Bounds.BorderBottom Style.PADDING) Colors.black
            base.Draw()

    and OptionsPage() =
        inherit Page()

        let help_mode_info =
            Callout.Normal
                .Icon(Icons.INFO)
                .Title(%"options.ingame_help")
                .Body(%"options.ingame_help.hint")
                .Hotkey("tooltip")
        
        let options_home_page =
            page_container()
            |+ Dummy(NodeType.Leaf)
            |+ Text("Welcome back", Align = Alignment.LEFT).Pos(0, 4, PageWidth.Normal)
            |+ Text("More stuff coming to this page soon", Color = K Colors.text_subheading, Align = Alignment.LEFT).Pos(4, 2, PageWidth.Normal)
            |+ Callout.frame help_mode_info (fun (w, h) -> Position.SliceBottom(h).SliceLeft(w))
            |+ TileButton(
                sprintf "%s %s" Icons.ZAP (%"hud"),
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
            ).Pos(6)

        let page_body = SwapContainer(options_home_page)

        override this.Header() =
            OptionsHeader(
                fun (search_text: string) ->
                    if search_text = "" then
                        page_body.Current <- options_home_page
                    else
                        page_body.Current <- SearchResults.get search_text
            )

        override this.Content() = page_body

        override this.Title = sprintf "%s %s" Icons.SETTINGS (%"options")
        override this.OnClose() = ()

    let show () = Menu.ShowPage OptionsPage
