namespace Interlude.Features.Play

open System
open OpenTK
open Percyqaz.Common
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Charts.Tools
open Prelude.Charts.Tools.Patterns
open Prelude.Gameplay
open Prelude.Data.Content
open Interlude
open Interlude.Options
open Interlude.Content
open Interlude.UI
open Interlude.Features
open Interlude.Features.Online
open Interlude.Features.Play

module AutomaticSync =

    let offset =
        Setting.make
            (fun v ->
                Gameplay.Chart.SAVE_DATA.Value.Offset <- v + Gameplay.Chart.CHART.Value.FirstNote
                Song.set_local_offset v
            )
            (fun () -> Gameplay.Chart.SAVE_DATA.Value.Offset - Gameplay.Chart.CHART.Value.FirstNote)
        |> Setting.roundt 0

    let apply (scoring: IScoreMetric) =
        let mutable sum = 0.0f<ms>
        let mutable count = 1.0f

        for ev in scoring.HitEvents do
            match ev.Guts with
            | Hit x when not x.Missed ->
                sum <- sum + x.Delta
                count <- count + 1.0f
            | _ -> ()

        let mean = sum / count * Gameplay.rate.Value

        let first_note = Gameplay.Chart.CHART.Value.FirstNote

        let reccommended_offset =
            if count < 10.0f then
                offset.Value
            else
                Gameplay.Chart.SAVE_DATA.Value.Offset - first_note - mean * 1.25f

        offset.Set reccommended_offset

type Timeline(chart: ModChart, on_seek: Time -> unit) =
    inherit StaticWidget(NodeType.None)
    
    let HEIGHT = 60.0f

    // chord density is notes per second but n simultaneous notes count for 1 instead of n
    let samples = int ((chart.LastNote - chart.FirstNote) / 1000.0f) |> max 10 |> min 400
    let note_density, chord_density = Analysis.nps_cps samples chart

    let note_density, chord_density =
        Array.map float32 note_density, Array.map float32 chord_density

    let max_note_density = Array.max note_density

    override this.Draw() =
        let b = this.Bounds.Shrink(10.0f, 20.0f)
        let start = chart.FirstNote - Song.LEADIN_TIME
        let offset = b.Width * Song.LEADIN_TIME / chart.LastNote

        let w = (b.Width - offset) / float32 note_density.Length

        let mutable x = b.Left + offset - w
        let mutable note_prev = 0.0f
        let mutable chord_prev = 0.0f

        let chord_density_color = !*Palette.HIGHLIGHT_100

        for i = 0 to note_density.Length - 1 do
            let note_next = HEIGHT * note_density.[i] / max_note_density
            let chord_next = HEIGHT * chord_density.[i] / max_note_density

            Draw.quad 
                (
                    Quad.createv 
                        (x, b.Bottom)
                        (x, b.Bottom - note_prev)
                        (x + w, b.Bottom - note_next)
                        (x + w, b.Bottom)
                )
                (Quad.color Colors.white.O2)
                Sprite.DEFAULT_QUAD

            Draw.quad 
                (
                    Quad.createv 
                        (x, b.Bottom)
                        (x, b.Bottom - chord_prev)
                        (x + w, b.Bottom - chord_next)
                        (x + w, b.Bottom)
                )
                (Quad.color chord_density_color)
                Sprite.DEFAULT_QUAD

            x <- x + w
            note_prev <- note_next
            chord_prev <- chord_next
            
        Draw.quad 
            (
                Quad.createv 
                    (x, b.Bottom)
                    (x, b.Bottom - note_prev)
                    (b.Right, b.Bottom - note_prev)
                    (b.Right, b.Bottom)
            )
            (Quad.color Colors.white.O2)
            Sprite.DEFAULT_QUAD
            
        Draw.quad 
            (
                Quad.createv 
                    (x, b.Bottom)
                    (x, b.Bottom - chord_prev)
                    (b.Right, b.Bottom - chord_prev)
                    (b.Right, b.Bottom)
            )
            (Quad.color chord_density_color)
            Sprite.DEFAULT_QUAD

        let percent = (Song.time () - start) / (chart.LastNote - start) |> min 1.0f
        let x = b.Width * percent
        Draw.rect (b.SliceBottom(5.0f)) (Color.FromArgb(160, Color.White))
        Draw.rect (b.SliceBottom(5.0f).SliceLeft x) (Palette.color (255, 1.0f, 0.0f))

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if this.Bounds.Bottom - Mouse.y () < 200.0f && Mouse.left_click () then
            let percent =
                (Mouse.x () - 10.0f) / (Viewport.vwidth - 20.0f) |> min 1.0f |> max 0.0f

            let start = chart.FirstNote - Song.LEADIN_TIME
            let new_time = start + (chart.LastNote - start) * percent
            on_seek new_time

type ColumnLighting(keys, ns: NoteskinConfig, state) as this =
    inherit StaticWidget(NodeType.None)
    let sliders = Array.init keys (fun _ -> Animation.Fade 0.0f)
    let sprite = get_texture "receptorlighting"
    let light_time = Math.Max(0.0f, Math.Min(0.99f, ns.ColumnLightTime))

    let column_spacing = ns.KeymodeColumnSpacing keys
    let column_positions =
        let mutable x = 0.0f
        Array.init keys (fun i -> 
            let v = x
            if i + 1 < keys then x <- x + ns.ColumnWidth + column_spacing.[i]
            v)

    do
        let hitpos = float32 options.HitPosition.Value

        this.Position <-
            { Position.Default with
                Top = 0.0f %+ hitpos
                Bottom = 1.0f %- hitpos
            }

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        sliders |> Array.iter (fun s -> s.Update elapsed_ms)

        Array.iteri
            (fun k (s: Animation.Fade) ->
                if state.Scoring.KeyState |> Bitmask.has_key k then
                    s.Value <- 1.0f
            )
            sliders

    override this.Draw() =
        let threshold = 1.0f - light_time

        let f k (s: Animation.Fade) =
            if s.Value > threshold then
                let p = (s.Value - threshold) / light_time
                let a = 255.0f * p |> int

                Draw.sprite
                    (let x = ns.ColumnWidth * 0.5f + column_positions.[k]

                     if options.Upscroll.Value then
                         Sprite.aligned_box_x
                             (this.Bounds.Left + x, this.Bounds.Top, 0.5f, 1.0f, ns.ColumnWidth * p, -1.0f / p)
                             sprite
                     else
                         Sprite.aligned_box_x
                             (this.Bounds.Left + x, this.Bounds.Bottom, 0.5f, 1.0f, ns.ColumnWidth * p, 1.0f / p)
                             sprite)
                    (Color.FromArgb(a, Color.White))
                    sprite

        Array.iteri f sliders

type Explosions(keys, ns: NoteskinConfig, state: PlayState) as this =
    inherit StaticWidget(NodeType.None)

    let sliders = Array.init keys (fun _ -> Animation.Fade 0.0f)
    let timers = Array.zeroCreate keys
    let mem = Array.zeroCreate keys
    let holding = Array.create keys false
    let explode_time = Math.Clamp(ns.Explosions.FadeTime, 0f, 0.99f)
    let animation = Animation.Counter ns.Explosions.AnimationFrameTime
    let rotation = Noteskins.note_rotation keys
    
    let column_spacing = ns.KeymodeColumnSpacing keys
    let column_positions =
        let mutable x = 0.0f
        Array.init keys (fun i -> 
            let v = x
            if i + 1 < keys then x <- x + ns.ColumnWidth + column_spacing.[i]
            v)

    let handle_event (ev: HitEvent<HitEventGuts>) =
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

        this.Position <-
            { Position.Default with
                Top = 0.0f %+ hitpos
                Bottom = 1.0f %- hitpos
            }

        state.SubscribeToHits handle_event

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        animation.Update elapsed_ms
        sliders |> Array.iter (fun s -> s.Update elapsed_ms)

        for k = 0 to (keys - 1) do
            if holding.[k] && state.Scoring.KeyState |> Bitmask.has_key k |> not then
                holding.[k] <- false
                sliders.[k].Target <- 0.0f

    override this.Draw() =
        let columnwidth = ns.ColumnWidth
        let threshold = 1.0f - explode_time

        let f k (s: Animation.Fade) =
            if s.Value > threshold then
                let p = (s.Value - threshold) / explode_time
                let a = 255.0f * p |> int

                let box =
                    (if options.Upscroll.Value then
                         Rect.Box(
                             this.Bounds.Left + column_positions.[k],
                             this.Bounds.Top,
                             columnwidth,
                             columnwidth
                         )
                     else
                         Rect.Box(
                             this.Bounds.Left + column_positions.[k],
                             this.Bounds.Bottom - columnwidth,
                             columnwidth,
                             columnwidth
                         ))
                        .Expand((ns.Explosions.Scale - 1.0f) * columnwidth * 0.5f)
                        .Expand(ns.Explosions.ExpandAmount * (1.0f - p) * columnwidth)

                match mem.[k] with
                | Hit e ->
                    let color =
                        if ns.Explosions.Colors = ExplosionColors.Column then
                            k
                        else
                            match e.Judgement with
                            | Some j -> int j
                            | None -> 0

                    let frame =
                        (state.CurrentChartTime() - timers.[k])
                        / Time.ofFloat ns.Explosions.AnimationFrameTime
                        |> int

                    Draw.quad
                        (box.AsQuad |> rotation k)
                        (Quad.color (Color.FromArgb(a, Color.White)))
                        (Sprite.pick_texture
                            (frame, color)
                            (Content.get_texture (if e.IsHold then "holdexplosion" else "noteexplosion")))
                | _ -> ()

        Array.iteri f sliders

type LaneCover() =
    inherit StaticWidget(NodeType.None)

    override this.Draw() =

        if options.LaneCover.Enabled.Value then

            let bounds = this.Bounds.Expand(0.0f, 2.0f)
            let fade_length = options.LaneCover.FadeLength.Value

            let upper (amount: float32) =
                Draw.rect (bounds.SliceTop(amount - fade_length)) options.LaneCover.Color.Value

                Draw.quad
                    (bounds.SliceTop(amount).SliceBottom(fade_length).AsQuad)
                    struct (options.LaneCover.Color.Value,
                            options.LaneCover.Color.Value,
                            Color.FromArgb(0, options.LaneCover.Color.Value),
                            Color.FromArgb(0, options.LaneCover.Color.Value))
                    Sprite.DEFAULT_QUAD

            let lower (amount: float32) =
                Draw.rect (bounds.SliceBottom(amount - fade_length)) options.LaneCover.Color.Value

                Draw.quad
                    (bounds.SliceBottom(amount).SliceTop(fade_length).AsQuad)
                    struct (Color.FromArgb(0, options.LaneCover.Color.Value),
                            Color.FromArgb(0, options.LaneCover.Color.Value),
                            options.LaneCover.Color.Value,
                            options.LaneCover.Color.Value)
                    Sprite.DEFAULT_QUAD

            let height = bounds.Height

            let sudden = options.LaneCover.Sudden.Value * height
            let hidden = options.LaneCover.Hidden.Value * height

            if options.Upscroll.Value then
                upper hidden
                lower sudden
            else
                lower hidden
                upper sudden

[<AutoOpen>]
module Utils =

    let inline add_widget
        (screen: Screen, playfield: Playfield, state: PlayState)
        (constructor: 'T * PlayState -> #Widget)
        =
        let config: ^T = HUDOptions.get<'T> ()
        let pos: WidgetPosition = (^T: (member Position: WidgetPosition) config)

        if pos.Enabled then
            let w = constructor (config, state)

            w.Position <-
                {
                    Left = pos.LeftA %+ pos.Left
                    Top = pos.TopA %+ pos.Top
                    Right = pos.RightA %+ pos.Right
                    Bottom = pos.BottomA %+ pos.Bottom
                }

            if pos.Float then screen.Add w else playfield.Add w

[<AbstractClass>]
type IPlayScreen(chart: ModChart, pacemaker_info: PacemakerInfo, ruleset: Ruleset, scoring: IScoreMetric) as this =
    inherit Screen()

    let mutable first_note = chart.Notes.[0].Time

    let state: PlayState =
        {
            Ruleset = ruleset
            Scoring = scoring
            ScoringChanged = Event<unit>()
            CurrentChartTime = fun () -> Song.time_with_offset () - first_note
            Pacemaker = pacemaker_info
        }

    let playfield = Playfield(state, options.VanishingNotes.Value)

    do
        this.Add playfield

        if noteskin_config().EnableColumnLight then
            playfield.Add(new ColumnLighting(chart.Keys, noteskin_config (), state))

        if noteskin_config().Explosions.Enable then
            playfield.Add(new Explosions(chart.Keys, noteskin_config (), state))

        playfield.Add(LaneCover())

        this.AddWidgets()

    abstract member AddWidgets: unit -> unit

    member this.FirstNote
        with set (value) = first_note <- value

    member this.Playfield = playfield
    member this.State = state
    member this.Chart = chart

    override this.OnEnter(prev) =
        Dialog.close ()
        Background.dim (float32 options.BackgroundDim.Value)
        Toolbar.hide ()
        Song.change_rate Gameplay.rate.Value
        Song.set_global_offset (options.AudioOffset.Value * 1.0f<ms>)
        Song.on_finish <- SongFinishAction.Wait
        Song.play_leadin ()
        Input.remove_input_method ()
        Input.finish_frame_events ()

    override this.OnExit next =
        Background.dim 0.7f

        if next <> Screen.Type.Score then
            Toolbar.show ()

    override this.OnBack() =
        if Network.lobby.IsSome then
            Some Screen.Type.Lobby
        else
            Some Screen.Type.LevelSelect
