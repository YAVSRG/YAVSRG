namespace Interlude.UI

open Interlude
open Interlude.UI.Screens

module Startup =

    let init() =
        Screen.init [|LoadingScreen(); MainMenu(); Import.Screen(); LevelSelect.Screen()|]
        
        Score.WatchReplay.func <- fun data -> Screen.changeNew (fun () -> Play.Screen(Play.PlayScreenType.Replay data) :> Screen.T) Screen.Type.Play Screen.TransitionFlag.Default
        TaskDisplay.init()
        Utils.AutoUpdate.checkForUpdates()
        Import.Mounts.handleStartupImports()

        Screen.Container(Toolbar())