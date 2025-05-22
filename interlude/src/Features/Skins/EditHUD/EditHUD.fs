namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay.Replays
open Prelude.Gameplay.Scoring
open Prelude.Skins.Noteskins
open Interlude.Options
open Interlude.UI
open Interlude.Content
open Interlude.Features.Gameplay
open Interlude.Features.Online
open Interlude.Features.Play
open Interlude.Features.Pacemaker

module EditHudScreen =

    let edit_hud_screen (info: LoadedChartInfo, on_exit: unit -> unit) =

        let replay_data: IReplay =
            StoredReplay.WavingAutoPlay(info.WithColors.Keys, info.WithColors.Source.Notes)

        let FIRST_NOTE = info.WithColors.FirstNote
        let ruleset = Rulesets.current

        let scoring =
            ScoreProcessor.create ruleset info.WithColors.Keys replay_data info.WithColors.Source.Notes SelectedChart.rate.Value

        let mutable time = -Time.infinity

        let seek_backwards (screen: IPlayScreen) =
            screen.State.ChangeScoring (screen.State.Scoring.Recreate())

        let mutable ctx: PositionerContext = Unchecked.defaultof<_>

        { new IPlayScreen(info, PacemakerState.None, scoring) with
            override this.AddWidgets() =

                ctx <-
                    {
                        Screen = Container(NodeType.None)
                        Playfield = this.Playfield
                        State = this.State
                        Selected = None
                        Positioners = Map.empty
                        UndoHistory = []
                        OnElementMoved = Event<unit>()
                    }

                ctx.CreateAll()

                this
                |+ ctx.Screen
                |* HUDEditorControls ctx
            // todo: way to turn on multiplayer player list

            override this.OnEnter p =
                DiscordRPC.in_menus ("Customising HUD")
                Dialog.close ()
                Background.dim (float32 options.BackgroundDim.Value)
                Toolbar.hide ()
                Song.on_finish <- SongFinishAction.LoopFromBeginning
                Song.resume()
                Input.remove_listener ()

            override this.OnExit s =
                base.OnExit s
                if s <> ScreenType.Play then on_exit ()

            override this.Update(elapsed_ms, moved) =
                let chart_time = this.State.CurrentChartTime()

                if chart_time < time then
                    seek_backwards this

                time <- chart_time

                base.Update(elapsed_ms, moved)

                if Mouse.left_clicked() then
                    ctx.ClearSelection()

                this.State.Scoring.Update chart_time
        }