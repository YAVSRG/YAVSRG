namespace Interlude.UI

open Prelude.Common
open Interlude
open Interlude.UI.Screens
open Interlude.UI.Toolbar

module Startup =

    let init() =
        Screen.init [|MainMenu.LoadingScreen(); MainMenu.Screen(); Import.Screen(); LevelSelect.Screen()|]
        
        Score.Helpers.watchReplay <- fun (rate, data) -> Screen.changeNew (fun () -> Play.ReplayScreen(Play.ReplayMode.Replay (rate, data)) :> Screen.T) Screen.Type.Play Screen.TransitionFlag.Default
        TaskDisplay.init()
        Utils.AutoUpdate.checkForUpdates()
        Import.Mounts.handleStartupImports()
        
        Logging.Subscribe
            ( fun (level, main, details) ->
                sprintf "[%A] %s" level main |> Terminal.add_message )

        Screen.Container(Toolbar())