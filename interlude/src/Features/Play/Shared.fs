namespace Interlude.Features.Play

open OpenTK
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Charts.Formats.Interlude
open Prelude.Scoring
open Prelude.Gameplay.Mods
open Prelude.Data.Themes
open Interlude.Options
open Interlude.Content
open Interlude.UI
open Interlude.Features
open Interlude.Features.Online
open Interlude.Features.Play.GameplayWidgets

[<AutoOpen>]
module Utils =

    let inline add_widget (screen: Screen, playfield: Playfield, state: PlayState) (constructor: 'T * PlayState -> #Widget) = 
        let config: ^T = getGameplayConfig<'T>()
        let pos: WidgetConfig = (^T: (member Position: WidgetConfig) config)
        if pos.Enabled then
            let w = constructor(config, state)
            w.Position <- { Left = pos.LeftA %+ pos.Left; Top = pos.TopA %+ pos.Top; Right = pos.RightA %+ pos.Right; Bottom = pos.BottomA %+ pos.Bottom }
            if pos.Float then screen.Add w else playfield.Add w

[<AbstractClass>]
type IPlayScreen(chart: ModChart, pacemakerInfo: PacemakerInfo, ruleset: Ruleset, scoring: IScoreMetric) as this =
    inherit Screen()
    
    let firstNote = offsetOf chart.Notes.First.Value

    let state: PlayState =
        {
            Ruleset = ruleset
            Scoring = scoring
            CurrentChartTime = fun () -> Song.timeWithOffset() - firstNote
            Pacemaker = pacemakerInfo
        }

    let playfield = Playfield state

    do
        this.Add playfield

        if noteskinConfig().EnableColumnLight then
            playfield.Add(new ColumnLighting(chart.Keys, noteskinConfig(), state))

        if noteskinConfig().Explosions.FadeTime >= 0.0f then
            playfield.Add(new Explosions(chart.Keys, noteskinConfig(), state))

        playfield.Add(LaneCover())

        this.AddWidgets()

    abstract member AddWidgets : unit -> unit

    member this.Playfield = playfield
    member this.State = state
    member this.Chart = chart

    override this.OnEnter(prev) =
        Dialog.close()
        Background.dim (float32 options.BackgroundDim.Value)
        Screen.Toolbar.hide()
        Song.changeRate Gameplay.rate.Value
        Song.changeGlobalOffset (options.AudioOffset.Value * 1.0f<ms>)
        Song.onFinish <- SongFinishAction.Wait
        Song.playLeadIn()
        Input.removeInputMethod()
        Input.finish_frame_events()

    override this.OnExit next =
        Background.dim 0.7f
        if next <> Screen.Type.Score then Screen.Toolbar.show()

    override this.OnBack() =
        if Network.lobby.IsSome then Some Screen.Type.Lobby
        else Some Screen.Type.LevelSelect