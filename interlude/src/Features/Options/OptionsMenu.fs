namespace Interlude.Features.OptionsMenu

open System
open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Interlude.Content
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Features.Stats
open Interlude.Features.Noteskins
open Interlude.Features.Noteskins.Edit
open Interlude.Features.Gameplay

type OptionsMenuPage() =
    inherit Page()

    let help_mode_info =
        Callout.Normal
            .Icon(Icons.INFO)
            .Title(%"options.ingame_help")
            .Body(%"options.ingame_help.hint")
            .Hotkey("tooltip")
        
    let options_home_page =
        NavigationContainer.Column(WrapNavigation = false, Position = Position.Margin(PRETTY_MARGIN_X, PRETTY_MARGIN_Y))
        |+ Dummy(NodeType.Leaf)
        |+ Text((fun () -> DateTime.Now.ToShortTimeString()), Align = Alignment.LEFT).Pos(0, 3, PageWidth.Normal)
        |+ Text((fun () -> [Stats.format_short_time Stats.session.GameTime; Stats.format_short_time Stats.session.PlayTime] %> "score.session_time"), Color = K Colors.text_subheading, Align = Alignment.LEFT).Pos(3, 2, PageWidth.Normal)
        |+ Callout.frame help_mode_info (fun (w, h) -> Position.SliceBottom(h).SliceLeft(w))
        // future quick actions: edit noteskin, edit hud, open wiki, view changelog, view stats, import stuff
        |+ OptionsMenuButton(
            sprintf "%s %s" Icons.ZAP (%"hud"),
            200.0f,
            (fun () -> 
                if Content.Noteskin.IsEmbedded then
                    EditHUDPage().Show()
                elif
                    SelectedChart.WITH_COLORS.IsSome
                    && Screen.change_new
                        (fun () -> HUDEditor.edit_hud_screen (SelectedChart.CHART.Value, SelectedChart.WITH_COLORS.Value, fun () -> OptionsMenuPage.Show()))
                        Screen.Type.Practice
                        Transitions.Default
                then
                    Menu.Exit()
            ),
            K false
        ).Pos(6)

    let page_body = SwapContainer(options_home_page)
    let mutable current_tab = OptionsMenuTab.Home
    let mutable on_destroy_current_tab = ignore
    let mutable on_return_current_tab = ignore

    let content_setting : Setting<OptionsMenuTab> = 
        Setting.make
            (fun new_tab -> 
                on_destroy_current_tab()
                current_tab <- new_tab
                match new_tab with
                | OptionsMenuTab.Home ->
                    on_destroy_current_tab <- ignore
                    on_return_current_tab <- ignore
                    page_body.Current <- options_home_page
                | OptionsMenuTab.System ->
                    let p = SystemSettings.SystemPage()
                    on_destroy_current_tab <- p.OnClose
                    on_return_current_tab <- p.OnReturnFromNestedPage
                    page_body.Current <- p.Content()
                | OptionsMenuTab.Gameplay ->
                    let p = Gameplay.GameplayPage()
                    on_destroy_current_tab <- p.OnDestroy
                    on_return_current_tab <- p.OnReturnFromNestedPage
                    page_body.Current <- p.Content()
                | OptionsMenuTab.Library ->
                    let p = Library.LibraryPage()
                    on_destroy_current_tab <- p.OnDestroy
                    on_return_current_tab <- p.OnReturnFromNestedPage
                    page_body.Current <- p.Content()
                | OptionsMenuTab.Noteskins ->
                    let p = SelectNoteskinsPage()
                    on_destroy_current_tab <- p.OnDestroy
                    on_return_current_tab <- p.OnReturnFromNestedPage
                    page_body.Current <- p.Content()
                | OptionsMenuTab.SearchResults contents ->
                    on_destroy_current_tab <- ignore
                    on_return_current_tab <- ignore
                    page_body.Current <- contents
            )
            (fun () -> current_tab)

    let header = 
        OptionsMenuHeader(content_setting)

    override this.Init(parent) =
        base.Init parent
        header.Focus false

    override this.Header() =
        header |> OverlayContainer :> Widget

    override this.Content() = page_body

    override this.Title = sprintf "%s %s" Icons.SETTINGS (%"options")
    override this.OnClose() = on_destroy_current_tab(); header.Hide()
    override this.OnEnterNestedPage() = header.Hide()
    override this.OnReturnFromNestedPage() = on_return_current_tab(); header.Show()

    static member Show() = Menu.ShowPage OptionsMenuPage
