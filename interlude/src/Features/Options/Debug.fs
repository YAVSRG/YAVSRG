namespace Interlude.Features.OptionsMenu

open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Data.Charts
open Interlude.Utils
open Interlude.Options
open Interlude.UI
open Interlude.UI.Menu

module Debug =

    type DebugPage() as this =
        inherit Page()

        do
            this.Content(
                column()
                |+ PrettyButton.Once(
                        "debug.rebuildcache",
                        fun () -> 
                            Library.recache_service.Request((), fun () -> Notifications.task_feedback(Icons.folder, L"notification.recache_complete", ""))
                            Notifications.action_feedback(Icons.folder, L"notification.recache", "") )
                    .Pos(200.0f)
                    .Tooltip(Tooltip.Info("options.debug.rebuildcache"))
                |+ PrettyButton.Once(
                        "debug.downloadupdate",
                        ( fun () ->
                            if AutoUpdate.updateAvailable then
                                AutoUpdate.applyUpdate(fun () -> Notifications.system_feedback(Icons.system_notification, L"notification.update_installed.title", L"notification.update_installed.body"))
                                Notifications.system_feedback(Icons.system_notification, L"notification.update_installing.title", L"notification.update_installing.body")
                        ),
                        Enabled = (AutoUpdate.updateAvailable && not AutoUpdate.updateDownloaded) )
                    .Pos(300.0f)
                |+ PrettySetting("debug.enableconsole", Selector<_>.FromBool options.EnableConsole)
                    .Pos(400.0f)
                    .Tooltip(Tooltip.Info("options.debug.enableconsole"))
            )
        override this.Title = L"options.debug.name"
        override this.OnClose() = ()