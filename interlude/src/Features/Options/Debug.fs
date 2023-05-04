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
        
        let refresh() =
            options.Theme.Value <- Themes.Current.id
            themes.Child <-
                Selector(Themes.list(), options.Theme |> Setting.trigger (fun id -> Themes.Current.switch id))
        
        let tryEditTheme() =
            let theme = Themes.Current.instance
            match theme.Source with
            | Zip (_, None) ->
                ConfirmPage(
                    Localisation.localiseWith [theme.Config.Name] "themes.confirmextractdefault",
                    (fun () -> 
                        if Themes.createNew(theme.Config.Name + "_extracted") then () 
                        else Logging.Error "Theme folder already exists"
                    )
                ).Show()
            | Folder _ -> EditThemePage().Show()
            | Zip (_, Some file) -> failwith "impossible as user themes are always folders"

        do
            refresh()

            this.Content(
                column()
                |+ PageButton.Once(
                        "debug.rebuildcache",
                        fun () -> 
                            Library.recache_service.Request((), fun () -> Notifications.task_feedback(Icons.folder, L"notification.recache_complete", ""))
                            Notifications.action_feedback(Icons.folder, L"notification.recache", "") )
                    .Pos(200.0f)
                    .Tooltip(Tooltip.Info("debug.rebuildcache"))
                |+ PageButton.Once(
                        "debug.downloadupdate",
                        ( fun () ->
                            if AutoUpdate.updateAvailable then
                                AutoUpdate.applyUpdate(fun () -> Notifications.system_feedback(Icons.system_notification, L"notification.update_installed.title", L"notification.update_installed.body"))
                                Notifications.system_feedback(Icons.system_notification, L"notification.update_installing.title", L"notification.update_installing.body")
                        ),
                        Enabled = (AutoUpdate.updateAvailable && not AutoUpdate.updateDownloaded) )
                    .Pos(300.0f)
                |+ PageSetting("debug.enableconsole", Selector<_>.FromBool options.EnableConsole)
                    .Pos(400.0f)
                    .Tooltip(Tooltip.Info("debug.enableconsole"))
                |+ themes
                    .Pos(500.0f)
                    .Tooltip(Tooltip.Info("themes.theme"))
                |+ PageButton("themes.edittheme", tryEditTheme)
                    .Pos(570.0f)
                    .Tooltip(Tooltip.Info("themes.edittheme"))
                |+ PageButton("themes.showthemesfolder", fun () -> openDirectory (getDataPath "Themes"))
                    .Pos(640.0f)
                    .Tooltip(Tooltip.Info("themes.showthemesfolder"))
            )
        override this.Title = L"debug.name"
        override this.OnClose() = ()