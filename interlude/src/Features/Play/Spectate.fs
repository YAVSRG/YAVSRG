namespace Interlude.Features.Play

open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Charts.Tools
open Interlude.Content
open Interlude.UI
open Interlude.Features
open Interlude.Features.Online
open Interlude.Features.Play.HUD

module SpectateScreen =

    type Controls() =
        inherit StaticContainer(NodeType.None)

        override this.Init(parent) =
            // todo: controls for choosing who to spec
            base.Init parent

        override this.Draw() =
            Draw.rect this.Bounds Colors.black.O2
            base.Draw()

    type ControlOverlay(on_seek) =
        inherit DynamicContainer(NodeType.None)

        let mutable show = true
        let mutable show_timeout = 3000.0

        override this.Init(parent) =
            this
            |+ Timeline(Gameplay.Chart.current.Value, on_seek)
            |* Controls(Position = Position.Box(0.0f, 0.0f, 30.0f, 70.0f, 440.0f, 60.0f))
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
        let scoring = fst Gameplay.Online.Multiplayer.replays.[username]
        let replay_data = Network.lobby.Value.Players.[username].Replay

        let firstNote = chart.Notes.[0].Time
        let ruleset = Rulesets.current

        Lobby.start_spectating()

        let handler = Network.Events.game_end.Subscribe (fun () -> Screen.back Transitions.Flags.Default)

        { new IPlayScreen(chart, PacemakerInfo.None, ruleset, scoring) with
            override this.AddWidgets() =
                let inline add_widget x = add_widget (this, this.Playfield, this.State) x
                
                add_widget ComboMeter
                add_widget SkipButton
                add_widget ProgressMeter
                add_widget AccuracyMeter
                add_widget HitMeter
                add_widget LifeMeter
                add_widget JudgementCounts
                add_widget JudgementMeter
                add_widget EarlyLateMeter
                add_widget MultiplayerScoreTracker

                this
                |* ControlOverlay(fun t -> Song.seek t)

            override this.OnEnter(prev) =
                base.OnEnter(prev)
                Song.onFinish <- SongFinishAction.Wait

            override this.OnExit(next) =
                base.OnExit(next)
                handler.Dispose()

            override this.Update(elapsedTime, bounds) =
                base.Update(elapsedTime, bounds)
                let now = Song.timeWithOffset()
                let chartTime = now - firstNote

                if replay_data.Time() - chartTime < 3000.0f<ms> then
                    if Song.playing() then Song.pause()
                elif not (Song.playing()) then Song.resume()
            
                scoring.Update chartTime
        }