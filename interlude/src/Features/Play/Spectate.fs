namespace Interlude.Features.Play

open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Charts.Tools
open Interlude.Web.Shared.Packets
open Interlude.Content
open Interlude.Utils
open Interlude.UI
open Interlude.Features
open Interlude.Features.Online
open Interlude.Features.Play.HUD

module SpectateScreen =

    type Controls(who: unit -> string, cycle: unit -> unit) =
        inherit StaticContainer(NodeType.None)

        override this.Init(parent) =
            this
            |+ Text("Currently spectating", Color = K Colors.text_subheading, Align = Alignment.CENTER, Position = Position.SliceTop(40.0f))
            |+ Text(who, Color = K Colors.text, Align = Alignment.CENTER, Position = Position.TrimTop(40.0f))
            |* Clickable(cycle)
            base.Init parent

        override this.Draw() =
            Draw.rect this.Bounds Colors.black.O2
            base.Draw()

    type ControlOverlay(on_seek, who, cycle) =
        inherit DynamicContainer(NodeType.None)

        let mutable show = true
        let mutable show_timeout = 3000.0

        override this.Init(parent) =
            this
            |+ Timeline(Gameplay.Chart.current.Value, on_seek)
            |* Controls(who, cycle, Position = Position.Box(0.0f, 0.0f, 30.0f, 70.0f, 440.0f, 100.0f))
            base.Init parent

        override this.Update(elapsedTime, moved) =
            base.Update(elapsedTime, moved)
            if Mouse.moved_recently() then
                show <- true
                this.Position <- Position.Default
                show_timeout <- 1500.0
            elif show then
                show_timeout <- show_timeout - elapsedTime
                if show_timeout < 0.0 then
                    show <- false
                    this.Position <- { Position.Default with Top = 0.0f %- 300.0f; Bottom = 1.0f %+ 100.0f }

    let spectate_screen(username: string) =

        let chart = Gameplay.Chart.withMods.Value

        let mutable currently_spectating = username
        let mutable scoring = fst Gameplay.Online.Multiplayer.replays.[username]
        let mutable replay_data = Network.lobby.Value.Players.[username].Replay

        let cycle_spectator(screen: IPlayScreen) =
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
            scoring <- fst Gameplay.Online.Multiplayer.replays.[next_user]
            replay_data <- Network.lobby.Value.Players.[next_user].Replay
            Song.seek(replay_data.Time() - MULTIPLAYER_REPLAY_DELAY_MS * 1.0f<ms>)
            screen.State.ChangeScoring scoring

        let firstNote = chart.Notes.[0].Time
        let lastNote = chart.Notes.[chart.Notes.Length - 1].Time
        let ruleset = Rulesets.current

        let mutable wait_for_load = 1000.0
        let mutable exiting = false

        Lobby.start_spectating()

        { new IPlayScreen(chart, PacemakerInfo.None, ruleset, scoring) with
            override this.AddWidgets() =
                let inline add_widget x = add_widget (this, this.Playfield, this.State) x
                
                add_widget ComboMeter
                add_widget SkipButton
                add_widget ProgressMeter
                add_widget AccuracyMeter
                add_widget HitMeter
                add_widget JudgementCounts
                add_widget JudgementMeter
                add_widget EarlyLateMeter
                add_widget MultiplayerScoreTracker

                this
                |* ControlOverlay(ignore, (fun () -> currently_spectating), fun () -> if Network.lobby.IsSome then cycle_spectator this)

            override this.OnEnter(prev) =
                base.OnEnter(prev)
                DiscordRPC.playing("Spectating", Gameplay.Chart.cacheInfo.Value.Title)
                Song.pause()

            override this.OnExit(next) =
                base.OnExit(next)
                Song.resume()

            override this.Update(elapsedTime, bounds) =
                base.Update(elapsedTime, bounds)

                if wait_for_load > 0.0 then
                    wait_for_load <- wait_for_load - elapsedTime
                    if wait_for_load <= 0 then
                        Song.seek(replay_data.Time() - MULTIPLAYER_REPLAY_DELAY_MS * 1.0f<ms>)
                        Song.resume()
                else

                let now = Song.timeWithOffset()
                let chartTime = now - firstNote

                if replay_data.Time() - chartTime < MULTIPLAYER_REPLAY_DELAY_MS * 1.0f<ms> then
                    if Song.playing() then Song.pause()
                elif not (Song.playing()) then Song.resume()

                scoring.Update chartTime

                if this.State.Scoring.Finished && not exiting then
                    exiting <- true
                    Screen.back Transitions.Flags.Default
        }