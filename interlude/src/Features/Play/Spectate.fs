namespace Interlude.Features.Play

open Percyqaz.Common
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Interlude.Web.Shared.Packets
open Interlude.Content
open Interlude.UI
open Interlude.Features
open Interlude.Features.Gameplay.Chart
open Interlude.Features.Online
open Interlude.Features.Play.HUD

module SpectateScreen =

    type Controls(who: unit -> string, cycle: unit -> unit) =
        inherit StaticContainer(NodeType.None)

        override this.Init(parent) =
            this
            |+ Text(
                "Currently spectating",
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
        inherit DynamicContainer(NodeType.None)

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

    let spectate_screen (info: LoadedChartInfo, username: string) =

        let mutable currently_spectating = username
        let mutable scoring = fst Gameplay.Multiplayer.replays.[username]
        let mutable replay_data = Network.lobby.Value.Players.[username].Replay

        let cycle_spectator (screen: IPlayScreen) =
            let users_available_to_spectate =
                let players = Network.lobby.Value.Players

                players.Keys
                |> Seq.filter (fun p -> players.[p].Status = LobbyPlayerStatus.Playing)
                |> Array.ofSeq

            let next_user =
                match Array.tryFindIndex (fun u -> u = currently_spectating) users_available_to_spectate with
                | None -> users_available_to_spectate.[0]
                | Some i -> users_available_to_spectate.[(i + 1) % users_available_to_spectate.Length]

            currently_spectating <- next_user
            scoring <- fst Gameplay.Multiplayer.replays.[next_user]
            replay_data <- Network.lobby.Value.Players.[next_user].Replay
            Song.seek (replay_data.Time() - MULTIPLAYER_REPLAY_DELAY_MS * 1.0f<ms>)
            screen.State.ChangeScoring scoring

        let first_note = info.WithMods.FirstNote

        let mutable wait_for_load = 1000.0
        let mutable exiting = false

        Lobby.start_spectating ()

        { new IPlayScreen(info.Chart, info.WithColors, PacemakerInfo.None, scoring) with
            override this.AddWidgets() =
                let inline add_widget x =
                    add_widget (this, this.Playfield, this.State) x

                add_widget ComboMeter
                add_widget SkipButton
                add_widget ProgressMeter
                add_widget AccuracyMeter
                add_widget HitMeter
                add_widget JudgementCounts
                add_widget JudgementMeter
                add_widget EarlyLateMeter
                add_widget RateModMeter
                add_widget BPMMeter
                add_widget MultiplayerScoreTracker

                this
                |* ControlOverlay(
                    info,
                    ignore,
                    (fun () -> currently_spectating),
                    fun () ->
                        if Network.lobby.IsSome then
                            cycle_spectator this
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
