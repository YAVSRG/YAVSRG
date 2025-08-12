namespace Interlude.Features.OptionsMenu

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Interlude.UI
open Interlude.Content
open Interlude.Features.Skins
open Interlude.Features.Gameplay

type OptionsPage() =
    inherit Page()

    let page_body = SwapContainer()
    let mutable current_tab = OptionsTab.Gameplay
    let mutable on_destroy_current_tab = ignore
    let mutable on_return_current_tab = ignore

    let content_setting : Setting<OptionsTab> =
        Setting.make
            (fun new_tab ->
                if current_tab = new_tab && (match page_body.Current with :? Dummy -> false | _ -> true) then () else
                on_destroy_current_tab()
                current_tab <- new_tab
                match new_tab with
                | OptionsTab.System ->
                    let p = SystemSettings.SystemPage()
                    on_destroy_current_tab <- p.OnClose
                    on_return_current_tab <- p.OnReturnFromNestedPage
                    page_body.Current <- p.Content()
                    State.recent_tab <- new_tab
                | OptionsTab.Gameplay ->
                    let p = Gameplay.GameplayPage()
                    on_destroy_current_tab <- p.OnDestroy
                    on_return_current_tab <- p.OnReturnFromNestedPage
                    page_body.Current <- p.Content()
                    State.recent_tab <- new_tab
                | OptionsTab.Library ->
                    let p = Library.LibraryPage()
                    on_destroy_current_tab <- p.OnDestroy
                    on_return_current_tab <- p.OnReturnFromNestedPage
                    page_body.Current <- p.Content()
                    State.recent_tab <- new_tab
                | OptionsTab.Noteskins ->
                    let p = SelectSkinsPage()
                    on_destroy_current_tab <- p.OnDestroy
                    on_return_current_tab <- p.OnReturnFromNestedPage
                    page_body.Current <- p.Content()
                    State.recent_tab <- new_tab
                | OptionsTab.SearchResults contents ->
                    on_destroy_current_tab <- ignore
                    on_return_current_tab <- ignore
                    page_body.Current <- contents
            )
            (fun () -> current_tab)

    do content_setting.Set State.recent_tab

    let header = OptionsPageHeader(content_setting)

    override this.Init(parent: Widget) =
        this.OnClose(fun () -> on_destroy_current_tab(); header.Hide())
        base.Init parent
        header.Focus false

    override this.Header() =
        header
        |> OverlayContainer
        :> Widget

    override this.Footer() =
        OptionsPageFooter()
        |+ HotkeyListener("reload_content", fun () ->
            Menu.Exit()
            Themes.reload_current ()
            Skins.load()
            Rulesets.load()
            Notifications.action_feedback (Icons.CHECK, %"notification.reload_content", "")
            OptionsPage().Show()
            SkinPreview.RefreshAll()
        )
        |> OverlayContainer
        :> Widget

    override this.Content() =
        content_setting.Set State.recent_tab
        page_body

    override this.Title = sprintf "%s %s" Icons.SETTINGS (%"options")
    override this.OnEnterNestedPage() = header.Hide()
    override this.OnReturnFromNestedPage() = on_return_current_tab(); header.Show()