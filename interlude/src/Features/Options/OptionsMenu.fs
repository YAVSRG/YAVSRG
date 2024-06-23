namespace Interlude.Features.OptionsMenu

open System
open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data
open Prelude.Data.Library
open Prelude.Data.Library.Caching
open Interlude.Content
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Features.Stats
open Interlude.Features.Noteskins
open Interlude.Features.Noteskins.Edit
open Interlude.Features.Gameplay
open Interlude.Features.Wiki
open Interlude.Features.Import
open Interlude.Features.Import.osu
open Interlude.Features.Import.Etterna
open Interlude.Features.Tables.Browser

module Imports =
    
    let import_in_progress () =
        WebServices.download_file.Status <> Async.ServiceStatus.Idle
        || Imports.auto_convert.Status <> Async.ServiceStatus.Idle
        || Cache.recache_service.Status <> Async.ServiceStatus.Idle
        || TableDownloader.download_service.Status <> Async.ServiceStatus.Idle
        || osu.Scores.import_osu_scores_service.Status <> Async.ServiceStatus.Idle

type OptionsMenuPage() =
    inherit Page()

    let help_mode_info =
        Callout.Small
            .Icon(Icons.INFO)
            .Title(%"options.ingame_help")
            .Body(%"options.ingame_help.hint")
            .Hotkey("tooltip")

    let quick_actions =
        NavigationContainer.Column()
        |+ OptionsMenuButton(
            sprintf "%s %s" Icons.ZAP (%"hud"),
            0.0f,
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
            Position = Position.Row(70.0f, 60.0f)
        )
        |+ OptionsMenuButton(
            sprintf "%s %s" Icons.IMAGE (%"noteskins.edit"),
            0.0f,
            (fun () -> 
                if Content.Noteskin.IsEmbedded then
                    SelectNoteskinsPage().Show()
                else
                    EditNoteskinPage(false).Show()
            ),
            Position = Position.Row(145.0f, 60.0f)
        )
        |+ Text("Quick actions", Position = Position.SliceTop(60.0f))
        
        |+ Text(
            (fun () ->
                if Imports.import_in_progress () then
                    %"imports.in_progress"
                else
                    %"imports.not_in_progress"
            ),
            Color =
                (fun () ->
                    if Imports.import_in_progress () then
                        Colors.text_green
                    else
                        Colors.text_subheading
                ),
            Position = Position.Row(245.0f, 40.0f).Margin(20.0f, 0.0f)
        )
        |+ LoadingIndicator.Strip(
            Imports.import_in_progress,
            Position = Position.Row(285.0f, Style.PADDING).Margin(150.0f, 0.0f)
        )
        |+ Text(
            sprintf "%i charts installed" Content.Library.Cache.Entries.Count,
            Color = K Colors.text_subheading,
            Position = Position.Row(300.0f, 30.0f).Margin(20.0f, 0.0f)
        )
        |+ OptionsMenuButton(
            sprintf "%s %s" Icons.DOWNLOAD_CLOUD (%"imports.osu"),
            0.0f,
            (fun () -> BeatmapBrowserPage().Show()),
            Position = Position.Row(340.0f, 60.0f)
        )
        |+ OptionsMenuButton(
            sprintf "%s %s" Icons.DOWNLOAD_CLOUD (%"imports.etterna"),
            0.0f,
            (fun () -> EtternaPacksBrowserPage().Show()),
            Position = Position.Row(415.0f, 60.0f)
        )

        |+ OptionsMenuButton(
            sprintf "%s %s" Icons.TRENDING_UP (%"menu.stats"),
            0.0f,
            (fun () ->
                if Screen.change_new StatsScreen Screen.Type.Stats Transitions.Transition.Default then 
                    Menu.Exit()
            ),
            Position = Position.Row(550.0f, 60.0f)
        )
        |+ OptionsMenuButton(
            sprintf "%s %s" Icons.BOOK_OPEN (%"menu.wiki"),
            0.0f,
            (fun () -> WikiBrowserPage.Show()),
            Position = Position.Row(625.0f, 60.0f)
        )
        |+ OptionsMenuButton(
            sprintf "%s %s" Icons.STAR (%"menu.changelog"),
            0.0f,
            (fun () -> WikiBrowserPage.ShowChangelog()),
            Position = Position.Row(700.0f, 60.0f)
        )
        |>> (fun nt -> Container(nt, Position = { Position.Default with Left = 0.65f %+ 10.0f }))
        
    let options_home_page =
        NavigationContainer.Column(WrapNavigation = false, Position = Position.Margin(PRETTY_MARGIN_X, PRETTY_MARGIN_Y))
        |+ quick_actions
        |+ Text((fun () -> DateTime.Now.ToShortTimeString()), Align = Alignment.LEFT).Pos(0, 3, PageWidth.Normal)
        |+ Text((fun () -> [Stats.format_short_time Stats.session.GameTime; Stats.format_short_time Stats.session.PlayTime] %> "score.session_time"), Color = K Colors.text_subheading, Align = Alignment.LEFT).Pos(3, 2, PageWidth.Normal)
        |+ Callout.frame help_mode_info (fun (w, h) -> Position.SliceBottom(h).SliceLeft(w))

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
