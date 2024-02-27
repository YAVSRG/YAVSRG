namespace Interlude.Features.Play

open System
open Percyqaz.Common
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Charts
open Prelude.Charts.Tools
open Prelude.Charts.Tools.NoteColors
open Prelude.Charts.Tools.Patterns
open Prelude.Gameplay
open Prelude.Data.Content
open Prelude.Data.Scores
open Interlude.Options
open Interlude.Content
open Interlude.UI
open Interlude.Features
open Interlude.Features.Online
open Interlude.Features.Play

module LocalAudioSync =

    let offset_setting (chart: Chart) (save_data: ChartSaveData) =
        Setting.make
            (fun v ->
                save_data.Offset <- v + chart.FirstNote
                Song.set_local_offset v
            )
            (fun () -> save_data.Offset - chart.FirstNote)
        |> Setting.roundt 0

    let get_automatic (state: PlayState) (save_data: ChartSaveData) =
        let mutable sum = 0.0f<ms>
        let mutable count = 1.0f

        for ev in state.Scoring.HitEvents do
            match ev.Guts with
            | Hit x when not x.Missed ->
                sum <- sum + x.Delta
                count <- count + 1.0f
            | _ -> ()

        let mean = sum / count * Gameplay.rate.Value

        let first_note = state.Chart.FirstNote

        if count < 10.0f then
            save_data.Offset - first_note
        else
            save_data.Offset - first_note - mean * 1.25f

    let apply_automatic (state: PlayState) (save_data: ChartSaveData)=
        let setting = offset_setting state.Chart save_data
        setting.Value <- get_automatic state save_data

type Timeline(with_mods: ModdedChart, on_seek: Time -> unit) =
    inherit StaticWidget(NodeType.None)

    let HEIGHT = 60.0f

    // chord density is notes per second but n simultaneous notes count for 1 instead of n
    let samples =
        int ((with_mods.LastNote - with_mods.FirstNote) / 1000.0f) |> max 10 |> min 400

    let note_density, chord_density = Analysis.nps_cps samples with_mods

    let note_density, chord_density =
        Array.map float32 note_density, Array.map float32 chord_density

    let max_note_density = Array.max note_density

    override this.Draw() =
        let b = this.Bounds.Shrink(10.0f, 20.0f)
        let start = with_mods.FirstNote - Song.LEADIN_TIME
        let offset = b.Width * Song.LEADIN_TIME /with_mods.LastNote

        let w = (b.Width - offset) / float32 note_density.Length

        let mutable x = b.Left + offset - w
        let mutable note_prev = 0.0f
        let mutable chord_prev = 0.0f

        let chord_density_color = !*Palette.HIGHLIGHT_100

        for i = 0 to note_density.Length - 1 do
            let note_next = HEIGHT * note_density.[i] / max_note_density
            let chord_next = HEIGHT * chord_density.[i] / max_note_density

            Draw.untextured_quad
                (Quad.createv (x, b.Bottom) (x, b.Bottom - note_prev) (x + w, b.Bottom - note_next) (x + w, b.Bottom))
                (Quad.color Colors.white.O2)

            Draw.untextured_quad
                (Quad.createv (x, b.Bottom) (x, b.Bottom - chord_prev) (x + w, b.Bottom - chord_next) (x + w, b.Bottom))
                (Quad.color chord_density_color)

            x <- x + w
            note_prev <- note_next
            chord_prev <- chord_next

        Draw.untextured_quad
            (Quad.createv (x, b.Bottom) (x, b.Bottom - note_prev) (b.Right, b.Bottom - note_prev) (b.Right, b.Bottom))
            (Quad.color Colors.white.O2)

        Draw.untextured_quad
            (Quad.createv (x, b.Bottom) (x, b.Bottom - chord_prev) (b.Right, b.Bottom - chord_prev) (b.Right, b.Bottom))
            (Quad.color chord_density_color)

        let percent = (Song.time () - start) / (with_mods.LastNote - start) |> min 1.0f
        let x = b.Width * percent
        Draw.rect (b.SliceBottom(5.0f)) (Color.FromArgb(160, Color.White))
        Draw.rect (b.SliceBottom(5.0f).SliceLeft x) (Palette.color (255, 1.0f, 0.0f))

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if this.Bounds.Bottom - Mouse.y () < 200.0f && Mouse.left_click () then
            let percent =
                (Mouse.x () - 10.0f) / (Viewport.vwidth - 20.0f) |> min 1.0f |> max 0.0f

            let start = with_mods.FirstNote - Song.LEADIN_TIME
            let new_time = start + (with_mods.LastNote - start) * percent
            on_seek new_time

type ColumnLighting(keys, ns: NoteskinConfig, state) as this =
    inherit StaticWidget(NodeType.None)
    let timers = Array.init keys (fun _ -> Animation.Delay ns.ColumnLightDuration)
    let sprite = Content.Texture "receptorlighting"

    let column_spacing = ns.KeymodeColumnSpacing keys

    let column_positions =
        let mutable x = 0.0f

        Array.init
            keys
            (fun i ->
                let v = x

                if i + 1 < keys then
                    x <- x + ns.ColumnWidth + column_spacing.[i]

                v
            )

    do
        let hitpos = float32 options.HitPosition.Value

        this.Position <-
            { Position.Default with
                Top = 0.0f %+ hitpos
                Bottom = 1.0f %- hitpos
            }

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        timers |> Array.iter (fun s -> s.Update elapsed_ms)

        Array.iteri
            (fun k (s: Animation.Delay) ->
                if state.Scoring.KeyState |> Bitmask.has_key k then
                    s.Reset()
            )
            timers

    override this.Draw() =

        let draw_column k (s: Animation.Delay) =
            if not s.Complete then
                let percent_remaining = 1.0f - float32 (s.Elapsed / s.Interval) |> min 1.0f |> max 0.0f
                let a = 255.0f * percent_remaining |> int

                Draw.sprite
                    (let x = ns.ColumnWidth * 0.5f + column_positions.[k]

                     if options.Upscroll.Value then
                         Sprite.aligned_box_x
                             (this.Bounds.Left + x, this.Bounds.Top, 0.5f, 1.0f, ns.ColumnWidth * percent_remaining, -1.0f / percent_remaining)
                             sprite
                     else
                         Sprite.aligned_box_x
                             (this.Bounds.Left + x, this.Bounds.Bottom, 0.5f, 1.0f, ns.ColumnWidth * percent_remaining, 1.0f / percent_remaining)
                             sprite)
                    (Color.FromArgb(a, Color.White))
                    sprite

        Array.iteri draw_column timers

type Explosions(keys, ns: NoteskinConfig, state: PlayState) as this =
    inherit StaticWidget(NodeType.None)

    let note_colors : int array = Array.zeroCreate keys
    let hold_colors : int array = Array.zeroCreate keys
    let holding : bool array = Array.create keys false

    let note_durations = Array.init keys (fun _ -> Animation.Delay ns.NoteExplosionSettings.Duration)
    let note_frames = Array.init keys (fun _ -> Animation.Counter ns.NoteExplosionSettings.AnimationFrameTime)

    let hold_durations = Array.init keys (fun _ -> Animation.Delay ns.HoldExplosionSettings.Duration)
    let hold_frames = Array.init keys (fun _ -> Animation.Counter ns.HoldExplosionSettings.AnimationFrameTime)

    let release_durations = Array.init keys (fun _ -> Animation.Delay ns.ReleaseExplosionSettings.Duration)
    let release_frames = Array.init keys (fun _ -> Animation.Counter ns.ReleaseExplosionSettings.AnimationFrameTime)

    let release_settings = if ns.HoldExplosionSettings.UseBuiltInAnimation then ns.HoldExplosionSettings else ns.ReleaseExplosionSettings

    let update_animations elapsed_ms =
        for k = 0 to keys - 1 do
            note_durations.[k].Update elapsed_ms
            note_frames.[k].Update elapsed_ms

            hold_durations.[k].Update elapsed_ms
            hold_frames.[k].Update elapsed_ms

            release_durations.[k].Update elapsed_ms
            release_frames.[k].Update elapsed_ms

    let rotation = Noteskins.note_rotation keys

    let column_spacing = ns.KeymodeColumnSpacing keys

    let column_positions =
        let mutable x = 0.0f

        Array.init
            keys
            (fun i ->
                let v = x

                if i + 1 < keys then
                    x <- x + ns.ColumnWidth + column_spacing.[i]

                v
            )

    let handle_event (ev: HitEvent<HitEventGuts>) =
        match ev.Guts with
        | Hit e when not e.Missed ->
            note_colors.[ev.Column] <-
                match ns.NoteExplosionSettings.Colors with
                | ExplosionColors.Note -> int state.WithColors.Colors.[ev.Index].Data.[ev.Column]
                | ExplosionColors.Judgements -> e.Judgement |> Option.defaultValue -1
            note_durations.[ev.Column].Reset()
            note_frames.[ev.Column].Reset()

        | Hit e when (e.IsHold && not e.Missed) ->
            hold_colors.[ev.Column] <-
                match ns.HoldExplosionSettings.Colors with
                | ExplosionColors.Note -> int state.WithColors.Colors.[ev.Index].Data.[ev.Column]
                | ExplosionColors.Judgements -> e.Judgement |> Option.defaultValue -1
            hold_frames.[ev.Column].Reset()
            holding.[ev.Column] <- true

        | Release e when holding.[ev.Column] && not e.Missed ->
            hold_colors.[ev.Column] <-
                match release_settings.Colors with
                | ExplosionColors.Note -> hold_colors.[ev.Column]
                | ExplosionColors.Judgements -> e.Judgement |> Option.defaultValue -1
            release_durations.[ev.Column].Reset()
            release_frames.[ev.Column].Reset()
            holding.[ev.Column] <- false
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
        update_animations elapsed_ms

        for k = 0 to (keys - 1) do
            if holding.[k] && state.Scoring.KeyState |> Bitmask.has_key k |> not then
                hold_colors.[k] <-
                    match release_settings.Colors with
                    | ExplosionColors.Note -> hold_colors.[k]
                    | ExplosionColors.Judgements -> -1
                release_durations.[k].Reset()
                release_frames.[k].Reset()
                holding.[k] <- false

    override this.Draw() =
        ()
        //let columnwidth = ns.ColumnWidth
        //let threshold = 1.0f - explode_time

        //let draw_column k (s: Animation.Fade) =
        //    if s.Value > threshold then
        //        let p = (s.Value - threshold) / explode_time
        //        let a = 255.0f * p |> int

        //        let box =
        //            (if options.Upscroll.Value then
        //                 Rect.Box(this.Bounds.Left + column_positions.[k], this.Bounds.Top, columnwidth, columnwidth)
        //             else
        //                 Rect.Box(
        //                     this.Bounds.Left + column_positions.[k],
        //                     this.Bounds.Bottom - columnwidth,
        //                     columnwidth,
        //                     columnwidth
        //                 ))
        //                .Expand((ns.Explosions.Scale - 1.0f) * columnwidth * 0.5f)
        //                .Expand(ns.Explosions.ExpandAmount * (1.0f - p) * columnwidth)

        //        match mem.[k] with
        //        | Hit e ->
        //            let color =
        //                if ns.Explosions.Colors = ExplosionColors.Column then
        //                    k
        //                else
        //                    match e.Judgement with
        //                    | Some j -> int j
        //                    | None -> 0

        //            let frame =
        //                (state.CurrentChartTime() - timers.[k])
        //                / Time.ofFloat ns.Explosions.AnimationFrameTime
        //                |> int

        //            Draw.quad
        //                (box.AsQuad |> rotation k)
        //                (Quad.color (Color.FromArgb(a, Color.White)))
        //                (Sprite.pick_texture
        //                    (frame, color)
        //                    (Content.Texture (if e.IsHold then "holdexplosion" else "noteexplosion")))
        //        | _ -> ()

        //Array.iteri draw_column sliders

type LaneCover() =
    inherit StaticWidget(NodeType.None)

    override this.Draw() =

        if options.LaneCover.Enabled.Value then

            let bounds = this.Bounds.Expand(0.0f, 2.0f)
            let fade_length = options.LaneCover.FadeLength.Value

            let upper (amount: float32) =
                Draw.rect (bounds.SliceTop(amount - fade_length)) options.LaneCover.Color.Value

                Draw.untextured_quad
                    (bounds.SliceTop(amount).SliceBottom(fade_length).AsQuad)
                    struct (options.LaneCover.Color.Value,
                            options.LaneCover.Color.Value,
                            Color.FromArgb(0, options.LaneCover.Color.Value),
                            Color.FromArgb(0, options.LaneCover.Color.Value))

            let lower (amount: float32) =
                Draw.rect (bounds.SliceBottom(amount - fade_length)) options.LaneCover.Color.Value

                Draw.untextured_quad
                    (bounds.SliceBottom(amount).SliceTop(fade_length).AsQuad)
                    struct (Color.FromArgb(0, options.LaneCover.Color.Value),
                            Color.FromArgb(0, options.LaneCover.Color.Value),
                            options.LaneCover.Color.Value,
                            options.LaneCover.Color.Value)

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
type IPlayScreen(chart: Chart, with_colors: ColoredChart, pacemaker_info: PacemakerInfo, ruleset: Ruleset, scoring: IScoreMetric) as this =
    inherit Screen()

    let first_note = with_colors.FirstNote

    let state: PlayState =
        {
            Chart = chart
            WithColors = with_colors
            Ruleset = ruleset
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

        playfield.Add(LaneCover())

        this.AddWidgets()

    abstract member AddWidgets: unit -> unit

    member this.Playfield = playfield
    member this.State = state

    override this.OnEnter(prev) =
        Dialog.close ()
        Background.dim (float32 options.BackgroundDim.Value)
        Toolbar.hide ()
        Song.change_rate Gameplay.rate.Value
        Song.set_global_offset (options.AudioOffset.Value * 1.0f<ms>)
        Song.on_finish <- SongFinishAction.Wait
        Song.play_leadin ()
        Input.remove_listener ()
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

type SlideoutContent(content: Widget, height: float32) =
    inherit StaticContainer(NodeType.Container (fun () -> Some content))

    override this.Init(parent: Widget) =
        this.Add content
        base.Init parent

    interface DynamicSize with
        member this.OnSizeChanged with set _ = ()
        member this.Size = height

type Slideout(content: SlideoutContent) =
    inherit DynamicContainer(NodeType.None)

    let mutable is_open = false

    let MARGIN = 15.0f
    let height = (content :> DynamicSize).Size + MARGIN * 2.0f

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
            Draw.rect (this.Bounds.Expand(5.0f).SliceBottom(5.0f)) Colors.cyan_shadow
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
