namespace Interlude.UI

open Percyqaz.Common
open Percyqaz.Flux.Audio
open Prelude
open Prelude.Data.User
open Interlude
open Interlude.Content
open Interlude.Features.Import
open Interlude.Features.Gameplay
open Interlude.Features.Stats
open Interlude.Features.MainMenu
open Interlude.Features.Mounts
open Interlude.Features.LevelSelect
open Interlude.Features.Multiplayer
open Interlude.Features.Printerlude
open Interlude.Features.Toolbar
open Interlude.Features.Online
open Interlude.Features.Score

module Startup =

    let init_startup (instance) =
        Options.init_startup instance
        Options.Hotkeys.init_startup Options.options.Hotkeys
        Content.init_startup ()
        Stats.init_startup ()

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

        FileDrop.replay_dropped.Add(fun replay ->
            match SelectedChart.CACHE_DATA, SelectedChart.CHART with
            | Some cc, Some chart when Screen.current_type = Screen.Type.LevelSelect ->
                match osu.Replays.convert_replay_to_score replay chart with
                | Ok score ->
                    ConfirmPage("Is this replay for the chart currently selected?",
                        [|
                            "Yes, view it!", fun () -> 
                                Screen.change_new
                                    (fun () -> ScoreScreen(ScoreInfo.from_score cc chart Rulesets.current score, Gameplay.ImprovementFlags.None, false))
                                    Screen.Type.Score
                                    Transitions.EnterGameplayNoFadeAudio
                                |> ignore
                            %"confirm.no", ignore
                        |]
                    )
                        .Show()
                | Error reason -> ()
            | _ -> ()
        )

        Gameplay.watch_replay <- LevelSelect.watch_replay
        Gameplay.continue_endless_mode <- LevelSelect.continue_endless_mode
        Gameplay.retry <- fun () -> SelectedChart.if_loaded (LevelSelect.try_play >> ignore)

        Updates.check_for_updates ()

        { new Screen.ScreenRoot(Toolbar()) with
            override this.Init() =
                Printerlude.init_window (instance)
                Content.init_window ()
                DiscordRPC.init_window ()
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
