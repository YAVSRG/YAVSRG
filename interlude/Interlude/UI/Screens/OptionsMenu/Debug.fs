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
                let codepoint = Setting.simple 1
                column [
                    PrettyButton.Once(
                        "RebuildCache",
                        (fun () -> BackgroundTask.Create TaskFlags.LONGRUNNING "Rebuilding Cache" Library.rebuildTask |> ignore),
                        Localisation.localiseWith ["Rebuilding Cache"] "notification.TaskStarted", Task
                    ).Position(200.0f)
                    PrettyButton.Once(
                        "DownloadUpdate",
                        ( fun () ->
                            if AutoUpdate.updateAvailable then
                                AutoUpdate.applyUpdate(fun () -> Notification.add (Localisation.localise "notification.UpdateInstalled", System))
                        ),
                        Localisation.localise "notification.UpdateInstalling", System,
                        Enabled = AutoUpdate.updateAvailable
                    ).Position(300.0f)
                    PrettyButton("PatternTest", fun () -> Prelude.Editor.Patterns.Analysis.test Interlude.Gameplay.currentChart.Value).Position(400.0f)
                    PrettyButton("Font+", fun () -> codepoint.Value <- codepoint.Value + 1).Position(550.0f)
                    PrettyButton("Font-", fun () -> codepoint.Value <- codepoint.Value - 1).Position(650.0f)
                    Components.TextBox((fun () -> new string(seq { (codepoint.Value * 128) .. (codepoint.Value * 128 + 128) } |> Seq.map char |> Seq.toArray)), K Color.White, 0.5f)
                    |> position (WPos.bottomSlice 200.0f)
                    Components.TextBox((fun () -> string codepoint.Value), K Color.White, 0.5f)
                    |> position (WPos.topSlice 200.0f)
                    
                ] :> Selectable
            Callback = ignore
        }