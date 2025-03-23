namespace Interlude.Features.Play

open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Windowing
open Prelude.Charts
open Prelude.Gameplay.Scoring
open Prelude.Skins.Noteskins
open Prelude.Skins.HudLayouts
open Interlude.Options
open Interlude.Content
open Interlude.UI
open Interlude.Features.Gameplay
open Interlude.Features.Pacemaker
open Interlude.Features.Online
open Interlude.Features.Play

[<AutoOpen>]
module Utils =

    let inline add_widget
        (screen: Screen, playfield: Playfield, state: PlayState, config: HudConfig)
        (pos: HudPosition)
        (constructor: HudConfig * PlayState -> #Widget)
        =
            let w = constructor (config, state)

            w.Position <-
                {
                    Left = pos.Left
                    Top = pos.Top
                    Right = pos.Right
                    Bottom = pos.Bottom
                }

            if pos.RelativeToPlayfield then playfield.Add w else screen.Add w

[<AbstractClass>]
type IPlayScreen(chart: Chart, with_colors: ColoredChart, pacemaker_info: PacemakerState, scoring: ScoreProcessor) as this
    =
    inherit Screen()

    let first_note = with_colors.FirstNote

    let state: PlayState =
        {
            Chart = chart
            WithColors = with_colors
            Scoring = scoring
            ScoringChanged = Event<unit>()
            CurrentChartTime = fun () -> Song.time_with_offset () - first_note
            Pacemaker = pacemaker_info
        }

    let noteskin_config = Content.NoteskinConfig

    let playfield =
        Playfield(with_colors, state, noteskin_config, options.VanishingNotes.Value)

    do
        this.Add playfield

        playfield.Add(LanecoverOverReceptors())

        this.AddWidgets()

    abstract member AddWidgets: unit -> unit

    member this.Playfield = playfield
    member this.State = state

    override this.OnEnter(prev) =
        Dialog.close ()
        Background.dim (float32 options.BackgroundDim.Value)
        Toolbar.hide ()
        Song.change_rate SelectedChart.rate.Value
        Song.set_global_offset options.AudioOffset.Value
        Song.on_finish <- SongFinishAction.Wait
        Song.play_leadin with_colors.FirstNote
        Input.remove_listener ()
        Input.finish_frame_events ()
        WindowThread.defer WindowThread.disable_windows_key

    override this.OnExit next =
        Background.dim 0.7f

        if next <> ScreenType.Score then
            Toolbar.show ()

        if next <> ScreenType.Play then
            WindowThread.defer WindowThread.enable_windows_key

    override this.OnBack() =
        if Network.lobby.IsSome then
            Some ScreenType.Lobby
        else
            Some ScreenType.LevelSelect