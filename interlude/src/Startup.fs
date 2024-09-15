namespace Interlude.UI

open Percyqaz.Common
open Percyqaz.Flux.Audio
open Interlude
open Interlude.Content
open Interlude.Features.Gameplay
open Interlude.Features.Stats
open Interlude.Features.MainMenu
open Interlude.Features.Mounts
open Interlude.Features.LevelSelect
open Interlude.Features.Multiplayer
open Interlude.Features.Printerlude
open Interlude.Features.Toolbar
open Interlude.Features.Online

module Startup =
    let MIGRATION_VERSION = 2

    let migrate () =

        if Stats.total.MigrationVersion.IsNone then
            if Content.Charts.Entries.Count > 0 then
                Stats.total.MigrationVersion <- Some 0
            else
                Stats.total.MigrationVersion <- Some MIGRATION_VERSION

        match Stats.total.MigrationVersion with
        | None -> failwith "impossible"
        | Some i ->
            if i < 1 then
                // Originally a migration here for migrating to new hash format from before 0.7.2
                Stats.total.MigrationVersion <- Some 1

            if i < 2 then
                // Originally a migration here for generating pattern data
                Stats.total.MigrationVersion <- Some 2

    let init_startup (instance) =
        Options.init_startup instance
        Options.Hotkeys.init_startup Options.options.Hotkeys
        Stats.init_startup ()
        Content.init_startup ()

    let init_window (instance) =
        Screen.init_window
            [|
                LoadingScreen()
                MainMenuScreen()
                LobbyScreen()
                LevelSelectScreen()
            |]

        Devices.change_volume (Options.options.AudioVolume.Value, Options.options.AudioVolume.Value)
        Song.set_pitch_rates_enabled Options.options.AudioPitchRates.Value

        Gameplay.watch_replay <- LevelSelect.watch_replay
        Gameplay.continue_endless_mode <- LevelSelect.continue_endless_mode
        Gameplay.retry <- fun () -> SelectedChart.if_loaded (LevelSelect.try_play >> ignore)

        Updates.check_for_updates ()

        { new Screen.ScreenRoot(Toolbar()) with
            override this.Init() =
                Printerlude.init_window (instance)
                Content.init_window ()
                DiscordRPC.init_window ()
                migrate ()
                SelectedChart.init_window ()
                Network.init_window ()
                Mounts.init_window ()
                base.Init()
        }

    let mutable private has_shutdown = false

    type ShutdownType =
        | Normal
        | InternalCrash
        | ExternalCrash

    let deinit shutdown_type crash_splash =
        if has_shutdown then
            ()
        else
            has_shutdown <- true
            Stats.deinit ()
            Content.deinit ()
            Options.deinit ()
            Network.deinit ()
            Printerlude.deinit ()
            DiscordRPC.deinit ()

            match shutdown_type with
            | Normal -> Logging.Info("Thank you for playing")
            | InternalCrash -> 
                crash_splash ()
                System.Console.ReadLine() |> ignore
            | ExternalCrash ->
                crash_splash ()
                Logging.Critical("The game was abnormally force-quit, but was able to shut down correctly")

            Logging.Shutdown()
