namespace Interlude.UI

open Percyqaz.Common
open Percyqaz.Flux.Audio
open Prelude.Charts.Processing
open Prelude.Data
open Prelude.Data.Library.Caching
open Interlude
open Interlude.Content
open Interlude.Features
open Interlude.Features.Stats
open Interlude.Features.MainMenu
open Interlude.Features.Import
open Interlude.Features.Score
open Interlude.Features.LevelSelect
open Interlude.Features.Multiplayer
open Interlude.Features.Printerlude
open Interlude.Features.Toolbar
open Interlude.Features.Online
open Interlude.Features.Play

module Startup =
    let MIGRATION_VERSION = 2

    let migrate () =

        if Stats.total.MigrationVersion.IsNone then
            if Content.Cache.Entries.Count > 0 then
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
                ImportScreen()
                LobbyScreen()
                LevelSelectScreen()
            |]

        Devices.change_volume (Options.options.AudioVolume.Value, Options.options.AudioVolume.Value)

        ScoreScreenHelpers.watch_replay <-
            fun (score_info: ScoreInfo, with_colors: ColoredChart) ->
                if
                    Screen.change_new
                        (fun () ->
                            ReplayScreen.replay_screen (score_info.Chart, ReplayMode.Replay(score_info, with_colors))
                            :> Screen.T
                        )
                        Screen.Type.Replay
                        Transitions.Flags.Default
                then
                    Gameplay.rate.Value <- score_info.Rate

        OptionsMenu.Noteskins.Shared.choose_noteskins <-
            fun () -> OptionsMenu.Noteskins.NoteskinsPage().Show()

        Updates.check_for_updates ()

        { new Screen.ScreenRoot(Toolbar()) with
            override this.Init() =
                Printerlude.init_window (instance)
                Content.init_window ()
                DiscordRPC.init_window ()
                migrate ()
                Gameplay.init_window ()
                Network.init_window ()
                Mounts.import_mounts_on_startup ()
                base.Init()
        }

    let mutable private has_shutdown = false

    let deinit unexpected_shutdown crash_splash =
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

            if unexpected_shutdown then
                crash_splash ()
                Logging.Critical("The game crashed or quit abnormally, but was able to shut down correctly")
            else
                Logging.Info("Thank you for playing")

            Logging.Shutdown()
