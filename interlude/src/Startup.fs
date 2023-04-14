namespace Interlude.UI

open Percyqaz.Common
open Interlude
open Interlude.Features
open Interlude.Features.MainMenu
open Interlude.Features.Import
open Interlude.Features.Score
open Interlude.Features.Play
open Interlude.Features.LevelSelect
open Interlude.Features.Multiplayer
open Interlude.Features.Printerlude
open Interlude.Features.Toolbar

module Startup =

    let ui_entry_point() =
        Screen.init [|LoadingScreen(); MainMenuScreen(); ImportScreen(); LobbyScreen(); LevelSelectScreen()|]
        
        ScoreScreenHelpers.watchReplay <- fun (modchart, rate, data) -> Screen.changeNew (fun () -> ReplayScreen.replay_screen(ReplayMode.Replay (modchart, rate, data)) :> Screen.T) Screen.Type.Replay Transitions.Flags.Default
        Utils.AutoUpdate.checkForUpdates()
        Mounts.handleStartupImports()
        
        Logging.Subscribe
            ( fun (level, main, details) ->
                sprintf "[%A] %s" level main |> Terminal.add_message )

        { new Screen.ScreenRoot(Toolbar())
            with override this.Init() = Gameplay.init(); base.Init() }