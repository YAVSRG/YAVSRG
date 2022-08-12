namespace Interlude.UI

open Percyqaz.Common
open Interlude
open Interlude.UI.Screens
open Interlude.UI.Toolbar

module Startup =

    let init() =
        Screen.init [|MainMenu.LoadingScreen(); MainMenu.Screen(); Import.Screen(); LevelSelect.Screen()|]
        
        Score.Helpers.watchReplay <- fun (rate, data) -> Screen.changeNew (fun () -> Play.ReplayScreen(Play.ReplayMode.Replay (rate, data)) :> Screen.T) Screen.Type.Play Screen.TransitionFlags.Default
        TaskDisplay.init()
        Utils.AutoUpdate.checkForUpdates()
        Import.Mounts.handleStartupImports()
        
        Logging.Subscribe
            ( fun (level, main, details) ->
                sprintf "[%A] %s" level main |> Terminal.add_message )

        Screen.Container(Toolbar())

    // Eventually the screen container will replace this
    type Root() =
        inherit Percyqaz.Flux.UI.Root()

        let container = init()
        
        override this.Draw() = container.Draw()

        override this.Update(elapsedTime, moved) =
            container.Update(elapsedTime, moved)
            base.Update(elapsedTime, moved)
            if Screen.exit then this.ShouldExit <- true

        override this.Init() =
            base.Init()
            Gameplay.init()
            container.Init(this)