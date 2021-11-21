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
                    PrettyButton("RebuildCache", fun () -> BackgroundTask.Create TaskFlags.LONGRUNNING "Rebuilding Cache" Library.rebuildTask |> ignore).Position(200.0f)
                    PrettyButton("DownloadUpdate",
                        fun () ->
                            if Interlude.Utils.AutoUpdate.updateAvailable then
                                Interlude.Utils.AutoUpdate.applyUpdate(fun () -> Notification.add (Localisation.localise "notification.UpdateInstalled", System))
                    ).Position(300.0f)
                    PrettyButton("PatternTest", fun () -> Prelude.Editor.Patterns.Analysis.test Interlude.Gameplay.currentChart.Value).Position(400.0f)
                ] :> Selectable
            Callback = ignore
        }