namespace Interlude.UI

open Percyqaz.Common
open Interlude
open Interlude.UI.Features
open Interlude.UI.Toolbar

module Startup =

    let init() =
        Screen.init [|MainMenu.LoadingScreen(); MainMenu.MainMenuScreen(); Import.ImportScreen(); LevelSelect.LevelSelectScreen()|]
        
        Score.Helpers.watchReplay <- fun (rate, data) -> Screen.changeNew (fun () -> Play.ReplayScreen(Play.ReplayMode.Replay (rate, data)) :> Screen.T) Screen.Type.Play Screen.TransitionFlags.Default
        TaskDisplay.init()
        Utils.AutoUpdate.checkForUpdates()
        Import.Mounts.handleStartupImports()
        
        Logging.Subscribe
            ( fun (level, main, details) ->
                sprintf "[%A] %s" level main |> Terminal.add_message )

        Screen.Container(Toolbar())

    // todo: replace this with screen container (?)
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