namespace Interlude.Features.OptionsMenu

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Data.Content
open Prelude.Data.Charts
open Interlude.Utils
open Interlude.Options
open Interlude.Content
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Features.OptionsMenu.Themes

module Debug =

    type DebugPage() as this =
        inherit Page()

        let themes = PageSetting("themes.theme", Dummy())

        let refresh () =
            themes.Child <-
                Selector(Themes.list (), options.Theme)

        let tryEditTheme () =
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

            this.Content(
                column ()
                |+ PageButton
                    .Once(
                        "debug.rebuildcache",
                        fun () ->
                            Caching.Cache.recache_service.Request(
                                Library.cache,
                                fun () ->
                                    Notifications.task_feedback (Icons.FOLDER, %"notification.recache_complete", "")
                            )

                            Notifications.action_feedback (Icons.FOLDER, %"notification.recache", "")
                    )
                    .Pos(200.0f)
                    .Tooltip(Tooltip.Info("debug.rebuildcache"))
                |+ PageButton
                    .Once(
                        "debug.downloadupdate",
                        (fun () ->
                            if AutoUpdate.update_available then
                                AutoUpdate.apply_update (fun () ->
                                    Notifications.system_feedback (
                                        Icons.ALERT_OCTAGON,
                                        %"notification.update_installed.title",
                                        %"notification.update_installed.body"
                                    )
                                )

                                Notifications.system_feedback (
                                    Icons.ALERT_OCTAGON,
                                    %"notification.update_installing.title",
                                    %"notification.update_installing.body"
                                )
                        ),
                        Enabled = (AutoUpdate.update_available && not AutoUpdate.update_started)
                    )
                    .Pos(270.0f)
                |+ themes.Pos(580.0f).Tooltip(Tooltip.Info("themes.theme"))
                |+ PageButton("themes.edittheme", tryEditTheme)
                    .Pos(650.0f)
                    .Tooltip(Tooltip.Info("themes.edittheme"))
                |+ PageButton("themes.showthemesfolder", (fun () -> open_directory (get_game_folder "Themes")))
                    .Pos(720.0f)
                    .Tooltip(Tooltip.Info("themes.showthemesfolder"))
            )

        override this.Title = %"debug.name"
        override this.OnClose() = ()
