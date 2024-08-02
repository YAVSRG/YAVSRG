namespace Interlude.Features.Play

open Percyqaz.Common
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Windowing
open Prelude
open Prelude.Charts
open Prelude.Charts.Processing
open Prelude.Gameplay
open Prelude.Data
open Prelude.Skins.Noteskins
open Prelude.Skins.HudLayouts
open Interlude.Options
open Interlude.Content
open Interlude.UI
open Interlude.Features.Gameplay
open Interlude.Features.Pacemaker
open Interlude.Features.Online
open Interlude.Features.Play

module LocalAudioSync =

    let offset_setting (save_data: ChartSaveData) =
        Setting.make save_data.set_Offset save_data.get_Offset
        |> Setting.roundt 0
        |> Setting.trigger Song.set_local_offset

    let get_automatic (state: PlayState) (save_data: ChartSaveData) =
        let mutable sum = 0.0f<ms>
        let mutable count = 1.0f

        for ev in state.Scoring.HitEvents do
            match ev.Guts with
            | Hit x when not x.Missed ->
                sum <- sum + x.Delta
                count <- count + 1.0f
            | _ -> ()

        let mean = sum / count * SelectedChart.rate.Value

        if count < 10.0f then
            save_data.Offset
        else
            save_data.Offset - mean * 1.25f

    let apply_automatic (state: PlayState) (save_data: ChartSaveData) =
        let setting = offset_setting save_data
        setting.Value <- get_automatic state save_data

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
type IPlayScreen(chart: Chart, with_colors: ColoredChart, pacemaker_info: PacemakerState, scoring: IScoreMetric) as this
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

        if noteskin_config.EnableColumnLight then
            playfield.Add(new ColumnLighting(with_colors.Keys, noteskin_config, state))

        if noteskin_config.UseExplosions then
            playfield.Add(new Explosions(with_colors.Keys, noteskin_config, state))

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
        Song.set_global_offset (options.AudioOffset.Value * 1.0f<ms>)
        Song.on_finish <- SongFinishAction.Wait
        Song.play_leadin ()
        Input.remove_listener ()
        Input.finish_frame_events ()
        Window.defer Window.DisableWindowsKey

    override this.OnExit next =
        Background.dim 0.7f

        if next <> Screen.Type.Score then
            Toolbar.show ()

        if next <> Screen.Type.Play then
            Window.defer Window.EnableWindowsKey

    override this.OnBack() =
        if Network.lobby.IsSome then
            Some Screen.Type.Lobby
        else
            Some Screen.Type.LevelSelect

// todo: remove slideouts. local offset should just be a page and replay/practice modes can be a redesigned floating UI
type SlideoutContent(content: Widget, height: float32) =
    inherit Container(NodeType.Container(fun () -> Some content))

    override this.Init(parent: Widget) =
        this.Add content
        base.Init parent

    interface IHeight with
        member this.Height = height

type Slideout(content: SlideoutContent) =
    inherit SlideContainer(NodeType.None)

    let mutable is_open = false

    let MARGIN = 15.0f
    let height = (content :> IHeight).Height + MARGIN * 2.0f

    member val OnOpen = ignore with get, set
    member val OnClose = ignore with get, set
    member val AutoCloseWhen = fun (content: SlideoutContent) -> not content.Focused with get, set

    override this.Init(parent: Widget) =
        content.Position <- Position.Margin(MARGIN)
        content.Init this
        this.Position <- Position.SliceTop(height).Translate(0.0f, -height - 5.0f)

        base.Init parent

    member this.Close() =
        if is_open then
            is_open <- false
            this.Position <- Position.SliceTop(height).Translate(0.0f, -height - 5.0f)
            this.OnClose()

    member this.Open() =
        if not is_open then
            is_open <- true
            content.Focus false
            this.Position <- Position.SliceTop(height)
            this.OnOpen()

    override this.Draw() =
        if this.Bounds.Bottom > this.Parent.Bounds.Top - 4.0f then
            Draw.rect this.Bounds Colors.shadow_2.O2
            Draw.rect (this.Bounds.Expand(5.0f).SliceB(5.0f)) Colors.cyan_shadow
            content.Draw()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        let moved = moved || this.Moving

        if this.Bounds.Bottom > this.Parent.Bounds.Top - 4.0f then
            content.Update(elapsed_ms, moved)

        if is_open then
            if (%%"exit").Tapped() && content.Focused then
                Selection.up false

            if this.AutoCloseWhen content then
                this.Close()