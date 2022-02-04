namespace Interlude.UI.OptionsMenu

open Prelude.Common
open Prelude.Data.Charts
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Components.Selection
open Interlude.UI.Components.Selection.Menu

module Debug =

    let page() : SelectionPage =
        {
            Content = fun add ->
                column [
                    PrettyButton.Once(
                        "debug.rebuildcache",
                        (fun () -> BackgroundTask.Create TaskFlags.LONGRUNNING "Rebuilding Cache" Library.rebuildTask |> ignore),
                        Localisation.localiseWith ["Rebuilding Cache"] "notification.taskstarted", Task
                    ).Position(200.0f)
                    PrettyButton.Once(
                        "debug.downloadupdate",
                        ( fun () ->
                            if AutoUpdate.updateAvailable then
                                AutoUpdate.applyUpdate(fun () -> Notification.add (L"notification.update.installed", System))
                        ),
                        L"notification.update.installing", System,
                        Enabled = AutoUpdate.updateAvailable
                    ).Position(300.0f)
                    
                ] :> Selectable
            Callback = ignore
        }