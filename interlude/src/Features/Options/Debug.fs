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
                        (fun () -> Library.recache_service.Request((), ignore)),
                        Localisation.localiseWith ["Rebuilding Cache"] "notification.taskstarted", NotificationType.Task
                    ).Pos(200.0f)
                |+ PrettyButton.Once(
                        "debug.downloadupdate",
                        ( fun () ->
                            if AutoUpdate.updateAvailable then
                                AutoUpdate.applyUpdate(fun () -> Notifications.add (L"notification.update.installed", NotificationType.System))
                        ),
                        L"notification.update.installing", NotificationType.System,
                        Enabled = AutoUpdate.updateAvailable
                    ).Pos(300.0f)
                |+ PrettySetting("debug.enableconsole", Selector<_>.FromBool options.EnableConsole).Pos(400.0f)
            )
        override this.Title = N"debug"
        override this.OnClose() = ()