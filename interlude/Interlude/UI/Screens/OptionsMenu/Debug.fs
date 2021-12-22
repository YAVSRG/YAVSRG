namespace Interlude.UI.OptionsMenu

open Prelude.Common
open Prelude.Data.Charts
open Interlude.UI
open Interlude.UI.Components.Selection
open Interlude.UI.Components.Selection.Menu

module Debug =

    let icon = "⚒"
    let page() : SelectionPage =
        {
            Content = fun add ->
                column [
                    PrettyButton.Once(
                        "RebuildCache",
                        (fun () -> BackgroundTask.Create TaskFlags.LONGRUNNING "Rebuilding Cache" Library.rebuildTask |> ignore),
                        Localisation.localiseWith ["Rebuilding Cache"] "notification.TaskStarted", Task
                    ).Position(200.0f)
                    PrettyButton.Once(
                        "DownloadUpdate",
                        ( fun () ->
                            if Interlude.Utils.AutoUpdate.updateAvailable then
                                Interlude.Utils.AutoUpdate.applyUpdate(fun () -> Notification.add (Localisation.localise "notification.UpdateInstalled", System))
                        ),
                        Localisation.localise "notification.UpdateInstalling", System,
                        Enabled = Interlude.Utils.AutoUpdate.updateAvailable
                    ).Position(300.0f)
                    PrettyButton("PatternTest", fun () -> Prelude.Editor.Patterns.Analysis.test Interlude.Gameplay.currentChart.Value).Position(400.0f)
                ] :> Selectable
            Callback = ignore
        }