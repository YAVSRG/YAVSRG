namespace Interlude.UI

open Percyqaz.Common
open Percyqaz.Flux.Audio
open Prelude
open Prelude.Data.User
open Interlude.Options
open Interlude.Content
open Interlude.Features.Import
open Interlude.Features.Import.osu
open Interlude.Features.Gameplay
open Interlude.Features.MainMenu
open Interlude.Features.Mounts
open Interlude.Features.LevelSelect
open Interlude.Features.Multiplayer
open Interlude.Features.Printerlude
open Interlude.Features.Toolbar
open Interlude.Features.Online
open Interlude.Features.Score

module Startup =

    let mutable private deinit_required = false
    let mutable private deinit_once = false

    let init_startup (instance) =
        Options.init_startup ()
        Content.init_startup ()
        Stats.init_startup Content.Library Content.UserData

    let init_window (instance) =
        Screen.init_window
            [|
                LoadingScreen()
                MainMenuScreen()
                LobbyScreen()
                LevelSelectScreen()
            |]

        Audio.change_volume (options.AudioVolume.Value, options.AudioVolume.Value)
        Song.set_pitch_rates_enabled options.AudioPitchRates.Value

        Gameplay.watch_replay <- LevelSelect.watch_replay
        Gameplay.continue_endless_mode <- LevelSelect.continue_endless_mode
        Gameplay.retry <- fun () -> SelectedChart.if_loaded LevelSelect.play

        Interlude.Updates.check_for_updates ()
        Printerlude.init_window (instance)
        Content.init_window ()
        DiscordRPC.init_window ()
        SelectedChart.init_window ()
        Network.init_window ()
        Mounts.init_window ()

        deinit_required <- true

        Screen.ScreenRoot(Toolbar())

    type ShutdownType =
        | Normal
        | InternalCrash
        | ExternalCrash

    let deinit shutdown_type crash_splash =
        if deinit_once then
            ()
        else
            deinit_once <- true

            if deinit_required then
                Stats.save_current_session (Timestamp.now()) Content.UserData
                Content.deinit ()
                Options.deinit ()
                Network.deinit ()
                Printerlude.deinit ()
                DiscordRPC.deinit ()

            match shutdown_type with
            | Normal -> Logging.Info("Thank you for playing")
            | InternalCrash ->
                crash_splash ()
                Logging.Shutdown()
                Option.iter open_directory Logging.LogFile
            | ExternalCrash ->
                crash_splash ()
                Logging.Critical("The game was abnormally force-quit, but was able to shut down correctly")
                Logging.Shutdown()
            System.Environment.Exit(0)