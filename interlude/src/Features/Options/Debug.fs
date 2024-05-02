namespace Interlude.Features.OptionsMenu.Debug

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skinning
open Prelude.Data.Library
open Interlude.Options
open Interlude.Content
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Features.OptionsMenu.Themes

type DebugPage() as this =
    inherit Page()

    let themes = PageSetting("themes.theme", Dummy())

    let refresh () =
        themes.Child <- SelectDropdown(Themes.list (), options.Theme)

    let try_edit_theme () =
        let theme = Content.Theme

        match theme.Source with
        | Embedded _ ->
            ConfirmPage(
                [ theme.Config.Name ] %> "themes.confirmextractdefault",
                (fun () ->
                    if Themes.create_new (theme.Config.Name + "_extracted") then
                        ()
                    else
                        Logging.Error "Theme folder already exists"
                )
            )
                .Show()
        | Folder _ -> EditThemePage().Show()

    do
        refresh ()

        page_container()
        |+ PageButton
            .Once(
                "debug.rebuildcache",
                fun () ->
                    Caching.Cache.recache_service.Request(
                        Content.Cache,
                        fun () ->
                            Notifications.task_feedback (Icons.FOLDER, %"notification.recache_complete", "")
                    )

                    Notifications.action_feedback (Icons.FOLDER, %"notification.recache", "")
            )
            .Pos(0)
            .Tooltip(Tooltip.Info("debug.rebuildcache"))
        |+ themes
            .Pos(3)
            .Tooltip(Tooltip.Info("themes.theme"))
        |+ PageButton("themes.edittheme", try_edit_theme)
            .Pos(5)
            .Tooltip(Tooltip.Info("themes.edittheme"))
        |+ PageButton("themes.showthemesfolder", (fun () -> open_directory (get_game_folder "Themes")))
            .Pos(7)
            .Tooltip(Tooltip.Info("themes.showthemesfolder"))
        |> this.Content

    override this.Title = %"debug.name"
    override this.OnClose() = ()
