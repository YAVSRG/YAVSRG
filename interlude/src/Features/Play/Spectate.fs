namespace Interlude.Features.Play

open Percyqaz.Common
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Gameplay
open Interlude.Web.Shared.Packets
open Interlude.Content
open Interlude.Options
open Interlude.UI
open Interlude.Features.Pacemaker
open Interlude.Features.Gameplay
open Interlude.Features.Online
open Interlude.Features.Play.HUD

module SpectateScreen =

    type Controls(who: unit -> string, cycle: unit -> unit) =
        inherit Container(NodeType.None)

        override this.Init(parent) =
            this
            |+ Text(
                %"spectate.title",
                Color = K Colors.text_subheading,
                Align = Alignment.CENTER,
                Position = Position.SliceTop(40.0f)
            )
            |+ Text(who, Color = K Colors.text, Align = Alignment.CENTER, Position = Position.TrimTop(40.0f))
            |* Clickable(cycle)

            base.Init parent

        override this.Draw() =
            Draw.rect this.Bounds Colors.black.O2
            base.Draw()

    type ControlOverlay(info: LoadedChartInfo, on_seek, who, cycle) =
        inherit SlideContainer(NodeType.None)

        let mutable show = true
        let mutable show_timeout = 3000.0

        override this.Init(parent) =
            this |+ Timeline(info.WithMods, on_seek)
            |* Controls(who, cycle, Position = Position.Box(0.0f, 0.0f, 30.0f, 70.0f, 440.0f, 100.0f))

            base.Init parent

        override this.Update(elapsed_ms, moved) =
            base.Update(elapsed_ms, moved)

            if Mouse.moved_recently () then
                show <- true
                this.Position <- Position.Default
                show_timeout <- 1500.0
            elif show then
                show_timeout <- show_timeout - elapsed_ms

                if show_timeout < 0.0 then
                    show <- false

                    this.Position <-
                        { Position.Default with
                            Top = 0.0f %- 300.0f
                            Bottom = 1.0f %+ 100.0f
                        }

    let spectate_screen (info: LoadedChartInfo, username: string, replay_info: LobbyPlayerReplayInfo, lobby: Lobby) =

        let mutable currently_spectating = username
        let mutable scoring = replay_info.ScoreMetric
        let mutable replay_data : OnlineReplayProvider = replay_info.Replay :?> OnlineReplayProvider

        let cycle_spectator (screen: IPlayScreen) =
            let users_available_to_spectate =
                lobby.Replays.Keys
                |> Array.ofSeq

            let next_user =
                match Array.tryFindIndex (fun u -> u = currently_spectating) users_available_to_spectate with
                | None -> users_available_to_spectate.[0]
                | Some i -> users_available_to_spectate.[(i + 1) % users_available_to_spectate.Length]

            match lobby.GetReplayInfo next_user with
            | Some replay_info ->
                currently_spectating <- next_user
                scoring <- replay_info.ScoreMetric
                replay_data <- replay_info.Replay :?> OnlineReplayProvider
                Song.seek (replay_data.Time() - MULTIPLAYER_REPLAY_DELAY_MS * 1.0f<ms>)
                screen.State.ChangeScoring scoring
            | None -> Logging.Warn(sprintf "Failed to switch to replay data for %s" next_user)

        let first_note = info.WithMods.FirstNote

        let mutable wait_for_load = 1000.0
        let mutable exiting = false

        lobby.StartSpectating()

        { new IPlayScreen(info.Chart, info.WithColors, PacemakerState.None, scoring) with
            override this.AddWidgets() =
                let user_options = options.HUD.Value
                let noteskin_options = Content.NoteskinConfig.HUD
                let inline add_widget position constructor =
                    add_widget (this, this.Playfield, this.State, user_options, noteskin_options) position constructor

                if user_options.ComboEnabled then add_widget noteskin_options.ComboPosition Combo
                if user_options.ProgressMeterEnabled then add_widget noteskin_options.ProgressMeterPosition ProgressMeter
                if user_options.AccuracyEnabled then add_widget noteskin_options.AccuracyPosition Accuracy
                if user_options.TimingDisplayEnabled then add_widget noteskin_options.TimingDisplayPosition TimingDisplay
                if user_options.JudgementCounterEnabled then add_widget noteskin_options.JudgementCounterPosition JudgementCounter
                if user_options.JudgementMeterEnabled then add_widget noteskin_options.JudgementMeterPosition JudgementMeter
                if user_options.EarlyLateMeterEnabled then add_widget noteskin_options.EarlyLateMeterPosition EarlyLateMeter
                if user_options.RateModMeterEnabled then add_widget noteskin_options.RateModMeterPosition RateModMeter
                if user_options.BPMMeterEnabled then add_widget noteskin_options.BPMMeterPosition BPMMeter
                if user_options.InputMeterEnabled then add_widget noteskin_options.InputMeterPosition InputMeter
                add_widget noteskin_options.PacemakerPosition 
                    (fun (user_options, noteskin_options, state) -> MultiplayerScoreTracker(user_options, noteskin_options, state, lobby.Replays))

                this
                |* ControlOverlay(
                    info,
                    ignore,
                    (fun () -> currently_spectating),
                    fun () -> cycle_spectator this
                )

            override this.OnEnter(prev) =
                base.OnEnter(prev)
                DiscordRPC.playing ("Spectating", info.CacheInfo.Title)
                Song.pause ()

            override this.OnExit(next) =
                base.OnExit(next)
                Song.resume ()

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
                    Screen.back Transitions.Flags.Default |> ignore
        }
