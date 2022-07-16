namespace Interlude.UI

open Percyqaz.Common
open Percyqaz.Flux.Graphics
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

    type Root() =
        inherit Percyqaz.Flux.UI.Root()

        let container = init()
        
        override this.Draw() = container.Draw()

        override this.Update(elapsedTime, moved) =
            container.Update(elapsedTime, Viewport.bounds)