namespace Interlude.Features.OptionsMenu

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Interlude.UI
open Interlude.Features.Skins

type OptionsMenuPage() =
    inherit Page()

    let page_body = SwapContainer()
    let mutable current_tab = OptionsMenuTab.System
    let mutable on_destroy_current_tab = ignore
    let mutable on_return_current_tab = ignore

    let content_setting : Setting<OptionsMenuTab> =
        Setting.make
            (fun new_tab ->
                if current_tab = new_tab then () else
                on_destroy_current_tab()
                current_tab <- new_tab
                match new_tab with
                | OptionsMenuTab.System ->
                    let p = SystemSettings.SystemPage()
                    on_destroy_current_tab <- p.OnClose
                    on_return_current_tab <- p.OnReturnFromNestedPage
                    page_body.Current <- p.Content()
                    State.recent_tab <- new_tab
                | OptionsMenuTab.Gameplay ->
                    let p = Gameplay.GameplayPage()
                    on_destroy_current_tab <- p.OnDestroy
                    on_return_current_tab <- p.OnReturnFromNestedPage
                    page_body.Current <- p.Content()
                    State.recent_tab <- new_tab
                | OptionsMenuTab.Library ->
                    let p = Library.LibraryPage()
                    on_destroy_current_tab <- p.OnDestroy
                    on_return_current_tab <- p.OnReturnFromNestedPage
                    page_body.Current <- p.Content()
                    State.recent_tab <- new_tab
                | OptionsMenuTab.Noteskins ->
                    let p = SelectSkinsPage()
                    on_destroy_current_tab <- p.OnDestroy
                    on_return_current_tab <- p.OnReturnFromNestedPage
                    page_body.Current <- p.Content()
                    State.recent_tab <- new_tab
                | OptionsMenuTab.SearchResults contents ->
                    on_destroy_current_tab <- ignore
                    on_return_current_tab <- ignore
                    page_body.Current <- contents
            )
            (fun () -> current_tab)

    do content_setting.Set OptionsMenuTab.Gameplay

    let header = OptionsMenuHeader(content_setting)

    override this.Init(parent) =
        base.Init parent
        header.Focus false

    override this.Header() =
        header
        |> OverlayContainer
        :> Widget

    override this.Footer() =
        OptionsMenuFooter()
        |> OverlayContainer
        :> Widget

    override this.Content() =
        content_setting.Set State.recent_tab
        page_body

    override this.Title = sprintf "%s %s" Icons.SETTINGS (%"options")
    override this.OnClose() = on_destroy_current_tab(); header.Hide()
    override this.OnEnterNestedPage() = header.Hide()
    override this.OnReturnFromNestedPage() = on_return_current_tab(); header.Show()