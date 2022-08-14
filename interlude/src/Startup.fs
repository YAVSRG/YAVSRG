namespace Interlude.UI

open Percyqaz.Common
open Interlude
open Interlude.Features
open Interlude.Features.MainMenu
open Interlude.Features.Import
open Interlude.Features.Score
open Interlude.Features.Play
open Interlude.Features.LevelSelect
open Interlude.Features.Printerlude
open Interlude.Features.Toolbar

module Startup =

    let init() =
        Screen.init [|LoadingScreen(); MainMenuScreen(); ImportScreen(); LevelSelectScreen()|]
        
        ScoreScreenHelpers.watchReplay <- fun (rate, data) -> Screen.changeNew (fun () -> ReplayScreen(ReplayMode.Replay (rate, data)) :> Screen.T) Screen.Type.Play Transitions.Flags.Default
        TaskDisplay.init()
        Utils.AutoUpdate.checkForUpdates()
        Mounts.handleStartupImports()
        
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