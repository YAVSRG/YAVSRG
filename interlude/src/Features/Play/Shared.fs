namespace Interlude.Features.Play

open System
open OpenTK
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude.Common
open Prelude.Charts.Formats.Interlude
open Prelude.Scoring
open Prelude.Gameplay.Mods
open Prelude.Data.Content
open Interlude
open Interlude.Options
open Interlude.Content
open Interlude.UI
open Interlude.Features
open Interlude.Features.Online
open Interlude.Features.Play

type ColumnLighting(keys, ns: NoteskinConfig, state) as this =
    inherit StaticWidget(NodeType.None)
    let sliders = Array.init keys (fun _ -> Animation.Fade 0.0f)
    let sprite = getTexture "receptorlighting"
    let lightTime = Math.Max(0.0f, Math.Min(0.99f, ns.ColumnLightTime))

    do
        let hitpos = float32 options.HitPosition.Value
        this.Position <- { Position.Default with Top = 0.0f %+ hitpos; Bottom = 1.0f %- hitpos }

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        sliders |> Array.iter (fun s -> s.Update elapsedTime)
        Array.iteri (fun k (s: Animation.Fade) -> if state.Scoring.KeyState |> Bitmap.hasBit k then s.Value <- 1.0f) sliders

    override this.Draw() =
        let threshold = 1.0f - lightTime
        let f k (s: Animation.Fade) =
            if s.Value > threshold then
                let p = (s.Value - threshold) / lightTime
                let a = 255.0f * p |> int
                Draw.sprite
                    (
                        let x = ns.ColumnWidth * 0.5f + (ns.ColumnWidth + ns.ColumnSpacing) * float32 k
                        if options.Upscroll.Value then
                            Sprite.alignedBoxX(this.Bounds.Left + x, this.Bounds.Top, 0.5f, 1.0f, ns.ColumnWidth * p, -1.0f / p) sprite
                        else Sprite.alignedBoxX(this.Bounds.Left + x, this.Bounds.Bottom, 0.5f, 1.0f, ns.ColumnWidth * p, 1.0f / p) sprite
                    )
                    (Color.FromArgb(a, Color.White))
                    sprite
        Array.iteri f sliders

type Explosions(keys, ns: NoteskinConfig, state: PlayState) as this =
    inherit StaticWidget(NodeType.None)

    let sliders = Array.init keys (fun _ -> Animation.Fade 0.0f)
    let timers = Array.zeroCreate keys
    let mem = Array.zeroCreate keys
    let holding = Array.create keys false
    let explodeTime = Math.Min(0.99f, ns.Explosions.FadeTime)
    let animation = Animation.Counter ns.Explosions.AnimationFrameTime
    let rotation = Noteskins.noteRotation keys

    let handleEvent (ev: HitEvent<HitEventGuts>) =
        match ev.Guts with
        | Hit e when (ns.Explosions.ExplodeOnMiss || not e.Missed) ->
            sliders.[ev.Column].Target <- 1.0f
            sliders.[ev.Column].Value <- 1.0f
            timers.[ev.Column] <- ev.Time
            holding.[ev.Column] <- true
            mem.[ev.Column] <- ev.Guts
        | Hit e when (ns.Explosions.ExplodeOnMiss || not e.Missed) ->
            sliders.[ev.Column].Value <- 1.0f
            timers.[ev.Column] <- ev.Time
            mem.[ev.Column] <- ev.Guts
        | _ -> ()

    do
        let hitpos = float32 options.HitPosition.Value
        this.Position <- { Position.Default with Top = 0.0f %+ hitpos; Bottom = 1.0f %- hitpos }
        state.SubscribeToHits handleEvent

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        animation.Update elapsedTime
        sliders |> Array.iter (fun s -> s.Update elapsedTime)
        for k = 0 to (keys - 1) do
            if holding.[k] && state.Scoring.KeyState |> Bitmap.hasBit k |> not then
                holding.[k] <- false
                sliders.[k].Target <- 0.0f

    override this.Draw() =
        let columnwidth = ns.ColumnWidth
        let threshold = 1.0f - explodeTime
        let f k (s: Animation.Fade) =
            if s.Value > threshold then
                let p = (s.Value - threshold) / explodeTime
                let a = 255.0f * p |> int
                
                let box =
                    (
                        if options.Upscroll.Value then Rect.Box(this.Bounds.Left + (columnwidth + ns.ColumnSpacing) * float32 k, this.Bounds.Top, columnwidth, columnwidth)
                        else Rect.Box(this.Bounds.Left + (columnwidth + ns.ColumnSpacing) * float32 k, this.Bounds.Bottom - columnwidth, columnwidth, columnwidth)
                    )
                        .Expand((ns.Explosions.Scale - 1.0f) * columnwidth * 0.5f)
                        .Expand(ns.Explosions.ExpandAmount * (1.0f - p) * columnwidth, ns.Explosions.ExpandAmount * (1.0f - p) * columnwidth)
                match mem.[k] with
                | Hit e ->
                    let color = 
                        if ns.Explosions.Colors = ExplosionColors.Column then k
                        else match e.Judgement with Some j -> int j | None -> 0
                    let frame = (state.CurrentChartTime() - timers.[k]) / toTime ns.Explosions.AnimationFrameTime |> int
                    Draw.quad
                        (box |> Quad.ofRect |> rotation k)
                        (Quad.colorOf (Color.FromArgb(a, Color.White)))
                        (Sprite.gridUV (frame, color) (Content.getTexture (if e.IsHold then "holdexplosion" else "noteexplosion")))
                | _ -> ()
        Array.iteri f sliders

type LaneCover() =
    inherit StaticWidget(NodeType.None)

    override this.Draw() =
        
        if options.LaneCover.Enabled.Value then

            let bounds = this.Bounds.Expand(0.0f, 2.0f)
            let fadeLength = float32 options.LaneCover.FadeLength.Value
            let upper (amount: float32) =
                Draw.rect (bounds.SliceTop(amount - fadeLength)) options.LaneCover.Color.Value
                Draw.quad
                    (bounds.SliceTop(amount).SliceBottom(fadeLength) |> Quad.ofRect)
                    struct (options.LaneCover.Color.Value, options.LaneCover.Color.Value, Color.FromArgb(0, options.LaneCover.Color.Value), Color.FromArgb(0, options.LaneCover.Color.Value))
                    Sprite.DefaultQuad
            let lower (amount: float32) =
                Draw.rect (bounds.SliceBottom(amount - fadeLength)) options.LaneCover.Color.Value
                Draw.quad
                    (bounds.SliceBottom(amount).SliceTop(fadeLength) |> Quad.ofRect)
                    struct (Color.FromArgb(0, options.LaneCover.Color.Value), Color.FromArgb(0, options.LaneCover.Color.Value), options.LaneCover.Color.Value, options.LaneCover.Color.Value)
                    Sprite.DefaultQuad

            let height = bounds.Height

            let sudden = float32 options.LaneCover.Sudden.Value * height
            let hidden = float32 options.LaneCover.Hidden.Value * height

            if options.Upscroll.Value then upper hidden; lower sudden
            else lower hidden; upper sudden

[<AutoOpen>]
module Utils =

    let inline add_widget (screen: Screen, playfield: Playfield, state: PlayState) (constructor: 'T * PlayState -> #Widget) = 
        let config: ^T = HUDOptions.get<'T>()
        let pos: WidgetPosition = (^T: (member Position: WidgetPosition) config)
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
            ScoringChanged = Event<unit>()
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