namespace Interlude.Features.Play.Spectate

open Percyqaz.Common
open Percyqaz.Flux.Audio
open Prelude
open Prelude.Gameplay.Replays
open Interlude.Web.Shared.Packets
open Interlude.Content
open Interlude.UI
open Interlude.Features.Pacemaker
open Interlude.Features.Gameplay
open Interlude.Features.Online
open Interlude.Features.Play
open Interlude.Features.Play.HUD

module Spectate =

    let spectate_screen (info: LoadedChartInfo, username: string, replay_info: LobbyPlayerReplayInfo, lobby: Lobby) =

        let mutable currently_spectating = username
        let mutable scoring = replay_info.ScoreProcessor
        let mutable replay_data : OnlineReplayProvider = replay_info.Replay :?> OnlineReplayProvider

        let cycle_spectator (screen: IPlayScreen) =
            let users_available_to_spectate =
                lobby.Replays.Keys
                |> Seq.where (fun name -> name <> Network.credentials.Username)
                |> Array.ofSeq

            let next_user =
                match Array.tryFindIndex (fun u -> u = currently_spectating) users_available_to_spectate with
                | None -> users_available_to_spectate.[0]
                | Some i -> users_available_to_spectate.[(i + 1) % users_available_to_spectate.Length]

            match lobby.GetReplayInfo next_user with
            | Some replay_info ->
                currently_spectating <- next_user
                scoring <- replay_info.ScoreProcessor
                replay_data <- replay_info.Replay :?> OnlineReplayProvider
                Song.seek (replay_data.Time() - MULTIPLAYER_REPLAY_DELAY_MS * 1.0f<ms>)
                screen.State.ChangeScoring scoring
            | None -> Logging.Warn "Failed to switch to replay data for %s" next_user

        let first_note = info.WithMods.FirstNote

        let mutable wait_for_load = 1000.0
        let mutable exiting = false

        lobby.StartSpectating()

        { new IPlayScreen(info.Chart, info.WithColors, PacemakerState.None, scoring) with
            override this.AddWidgets() =
                let hud_config = Content.HUD
                let inline add_widget position constructor =
                    add_widget (this, this.Playfield, this.State, hud_config) position constructor

                if hud_config.ComboEnabled then add_widget hud_config.ComboPosition Combo
                if hud_config.ProgressMeterEnabled then add_widget hud_config.ProgressMeterPosition ProgressPie
                if hud_config.AccuracyEnabled then add_widget hud_config.AccuracyPosition Accuracy
                if hud_config.TimingDisplayEnabled then add_widget hud_config.TimingDisplayPosition ErrorBar
                if hud_config.JudgementCounterEnabled then add_widget hud_config.JudgementCounterPosition JudgementCounter
                if hud_config.JudgementMeterEnabled then add_widget hud_config.JudgementMeterPosition Judgement
                if hud_config.EarlyLateMeterEnabled then add_widget hud_config.EarlyLateMeterPosition EarlyLate
                if hud_config.RateModMeterEnabled then add_widget hud_config.RateModMeterPosition RateMods
                if hud_config.BPMMeterEnabled then add_widget hud_config.BPMMeterPosition BPM
                if hud_config.InputMeterEnabled then add_widget hud_config.InputMeterPosition InputMeter
                if hud_config.KeysPerSecondMeterEnabled then add_widget hud_config.KeysPerSecondMeterPosition KeysPerSecond
                if hud_config.CustomImageEnabled then add_widget hud_config.CustomImagePosition CustomImage
                add_widget hud_config.PacemakerPosition
                    (fun (hud_config, state) -> MultiplayerScoreTracker(hud_config, state, lobby.Replays))

                this
                |* ControlOverlay(
                    info,
                    ignore,
                    (fun () -> currently_spectating),
                    fun () -> cycle_spectator this
                )

            override this.OnEnter(prev) =
                base.OnEnter(prev)
                DiscordRPC.playing ("Spectating", info.ChartMeta.Title)
                Song.pause ()

            override this.OnExit(next) =
                base.OnExit(next)
                Song.resume ()
                Toolbar.show_cursor ()

            override this.Update(elapsed_ms, moved) =
                base.Update(elapsed_ms, moved)

                if wait_for_load > 0.0 then
                    wait_for_load <- wait_for_load - elapsed_ms

                    if wait_for_load <= 0 then
                        Song.seek (replay_data.Time() - MULTIPLAYER_REPLAY_DELAY_MS * 1.0f<ms>)
                        Song.resume ()
                else

                let now = Song.time_with_offset ()
                let chart_time = now - first_note

                if replay_data.Time() - chart_time < MULTIPLAYER_REPLAY_DELAY_MS * 1.0f<ms> then
                    if Song.playing () then
                        Song.pause ()
                elif not (Song.playing ()) then
                    Song.resume ()

                scoring.Update chart_time

                if this.State.Scoring.Finished && not exiting then
                    exiting <- true
                    Screen.back Transitions.Default |> ignore
        }