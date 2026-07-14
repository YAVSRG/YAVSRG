namespace Interlude.Features.Play

open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay.Scoring
open Prelude.Charts
open Prelude.Skins.Noteskins
open Interlude.Content
open Interlude.Options
open Interlude.Features.Gameplay

type private StageLeft() =
    inherit StaticWidget(NodeType.None)

    let sprite = Content.Texture "stageleft"

    override this.Draw() : unit =
        let width = sprite.AspectRatio * this.Bounds.Height

        Render.sprite
            (Rect.FromSize(this.Bounds.Left - width, this.Bounds.Top, width, this.Bounds.Height))
            Color.White
            sprite

type private StageRight() =
    inherit StaticWidget(NodeType.None)

    let sprite = Content.Texture "stageright"

    override this.Draw() : unit =
        let width = sprite.AspectRatio * this.Bounds.Height
        Render.sprite (Rect.FromSize(this.Bounds.Right, this.Bounds.Top, width, this.Bounds.Height)) Color.White sprite

[<Struct>]
type private HoldRenderState =
    | HeadOffscreen of index: int
    | HeadOnscreen of pos: float32 * index: int
    | NoHold

type Playfield(chart: ColoredChart, state: PlayState, noteskin_config: NoteskinConfig, vanishing_notes: bool) =
    inherit Container(NodeType.None)

    let NEGATIVE_SV_ROW_COUNT = 150

    let keys = chart.Keys

    let column_width = noteskin_config.KeymodeColumnWidth(keys)
    let column_spacing = noteskin_config.KeymodeColumnSpacing(keys)

    let calculate_column_left_edges () : float32 array =
        let left_edges = Array.zeroCreate(keys)
        let mutable x = 0.0f

        for key = 1 to keys - 1 do
            x <- x + column_width + column_spacing.[key - 1]
            left_edges.[key] <- x

        left_edges

    let column_positions = calculate_column_left_edges()

    let note_height = column_width
    let holdnote_trim = column_width * noteskin_config.HoldNoteTrim
    let playfield_color = noteskin_config.PlayfieldColor
    let fill_column_gaps = noteskin_config.FillColumnGaps
    let receptor_colors = noteskin_config.ReceptorColors.[keys - 3]

    let receptor = Content.Texture("receptor")
    let judgement_line = Content.Texture("judgementline")
    let holdtail = Content.Texture("holdtail")
    let holdhead = Content.Texture("holdhead")
    let holdbody = Content.Texture("holdbody")
    let note = Content.Texture("note")
    let animation = Animation.Counter(float noteskin_config.AnimationFrameTime)
    let receptor_aspect_ratio = receptor.AspectRatio

    let column_lighting = ColumnLighting(chart.Keys, noteskin_config, state)
    let explosions = Explosions(chart.Keys, noteskin_config, state)

    let sv = chart.SV
    let mutable note_seek = 0
    let mutable sv_seek = 0
    let holds_offscreen = Array.create keys -1
    let hold_draw_states = Array.create keys NoHold

    // todo: move to shared place as this is duplicate?
    let rotation: int -> Quad -> Quad =
        if noteskin_config.UseRotation then
            let rotations = noteskin_config.Rotations.[keys - 3]
            fun k -> Quad.rotate rotations.[k]
        else
            fun _ quad -> quad

    let mutable has_negative_sv = false
    let mutable last_time = -Time.infinity

    let handle_seek_back_in_time () : unit =
        note_seek <- 0
        sv_seek <- 0

        for k = 0 to hold_draw_states.Length - 1 do
            hold_draw_states.[k] <- NoHold
            holds_offscreen.[k] <- -1

    let scroll_direction_transform (bottom: float32) : Rect -> Rect =
        if options.Upscroll.Value then
            id
        else
            fun (r: Rect) ->
                {
                    Left = r.Left
                    Top = bottom - r.Bottom
                    Right = r.Right
                    Bottom = bottom - r.Top
                }

    let hold_tail_transform (k: int) : Quad -> Quad =
        if not(noteskin_config.UseHoldTailTexture) then rotation k
        elif not(noteskin_config.FlipHoldTail) || options.Upscroll.Value then id
        else Quad.flip_vertical

    let receptor_transform (k: int) : Quad -> Quad =
        if noteskin_config.ReceptorStyle = ReceptorStyle.Flip then
            if options.Upscroll.Value then Quad.flip_vertical else id
        else
            rotation k

    let judgement_line_transform: Quad -> Quad =
        if options.Upscroll.Value then Quad.flip_vertical else id

    member this.ColumnWidth = column_width
    member this.ColumnPositions = column_positions

    override this.Init(parent: Widget) : unit =
        let width = Array.mapi (fun _ n -> n + column_width) column_positions |> Array.max

        let screen_align_percentage, playfield_align_percentage =
            noteskin_config.PlayfieldAlignment

        if noteskin_config.EnableStageTextures then
            this.Add(StageLeft(), StageRight())

        this.Position <-
            {
                Left = screen_align_percentage %- (width * playfield_align_percentage)
                Top = Position.MIN
                Right = screen_align_percentage %+ (width * (1.0f - playfield_align_percentage))
                Bottom = Position.MAX
            }

        base.Init(parent)

        if noteskin_config.EnableColumnLight then
            column_lighting.Init(this)

        if noteskin_config.UseExplosions then
            explosions.Init(this)

    override this.Update(elapsed_ms: float, moved: bool) : unit =
        base.Update(elapsed_ms, moved)
        animation.Update(elapsed_ms)

        if noteskin_config.EnableColumnLight then
            column_lighting.Update(elapsed_ms, moved)

        if noteskin_config.UseExplosions then
            explosions.Update(elapsed_ms, moved)

    member private this.DrawPlayfieldBackground() : unit =
        if fill_column_gaps then
            Render.rect this.Bounds playfield_color
        else
            for k = 0 to keys - 1 do
                Render.rect_edges
                    (this.Bounds.Left + column_positions.[k])
                    this.Bounds.Top
                    (this.Bounds.Left + column_positions.[k] + column_width)
                    this.Bounds.Bottom
                    playfield_color

    member private this.DrawJudgementLine(hitposition: float32) : unit =
        if noteskin_config.UseJudgementLine then
            let area =
                Rect
                    .FromEdges(
                        this.Bounds.Left,
                        hitposition + (note_height - note_height * noteskin_config.JudgementLineScale) * 0.5f,
                        this.Bounds.Right,
                        hitposition + (note_height + note_height * noteskin_config.JudgementLineScale) * 0.5f
                    )
                    .TranslateY(note_height * noteskin_config.JudgementLineOffset)
                |> scroll_direction_transform this.Bounds.Bottom
                |> _.AsQuad
                |> judgement_line_transform

            Render.tex_quad area Color.White.AsQuad (Sprite.pick_texture (animation.Loops, 0) judgement_line)

    member private this.DrawReceptors(hitposition: float32) : unit =

        let inline get_receptor_texture (key: int) : QuadTexture =
            let is_pressed = state.Scoring.KeyState.Contains(key)
            let color_id = receptor_colors.[key] * 2 + (if is_pressed then 1 else 0)
            let frame_id = animation.Loops
            Sprite.pick_texture (frame_id, color_id) receptor

        let inline draw_receptor (key: int) : unit =
            let bounds =
                Rect
                    .FromSize(
                        this.Bounds.Left + column_positions.[key],
                        hitposition + note_height - note_height / receptor_aspect_ratio,
                        column_width,
                        note_height / receptor_aspect_ratio
                    )
                    .TranslateY(note_height * noteskin_config.ReceptorOffset)
                |> scroll_direction_transform this.Bounds.Bottom
                |> _.AsQuad
                |> receptor_transform key

            Render.tex_quad bounds Color.White.AsQuad (get_receptor_texture(key))

        if noteskin_config.UseReceptors then
            for k = 0 to keys - 1 do
                draw_receptor(k)

        if noteskin_config.EnableColumnLight then
            column_lighting.Draw()

    member private this.DrawNotes(hitposition: float32) : unit =
        let { Rect.Left = left; Top = top; Bottom = bottom } = this.Bounds

        let scroll_speed_scaled = options.ScrollSpeed.Value / SelectedChart.rate.Value
        let playfield_height = bottom - top + max 0.0f holdnote_trim

        let inline draw_note (key: int, pos: float32, color: int) : unit =
            let note_bounds =
                Rect.FromSize(left + column_positions.[key], pos, column_width, note_height)
                |> scroll_direction_transform bottom
                |> _.AsQuad
                |> rotation key

            Render.tex_quad note_bounds Color.White.AsQuad (Sprite.pick_texture (animation.Loops, color) note)

        let inline draw_head (key: int, pos: float32, color: int, tint: Color) : unit =
            let bounds =
                Rect.FromSize(left + column_positions.[key], pos, column_width, note_height)
                |> scroll_direction_transform bottom
                |> _.AsQuad
                |> rotation key

            Render.tex_quad bounds tint.AsQuad (Sprite.pick_texture (animation.Loops, color) holdhead)

        let inline draw_body (key: int, headpos: float32, tailpos: float32, color: int, tint: Color) : unit =
            if headpos <= tailpos then
                let bounds =
                    Rect.FromEdges(
                        left + column_positions.[key],
                        headpos + note_height * 0.5f - 1f,
                        left + column_positions.[key] + column_width,
                        tailpos + note_height * 0.5f + 1f |> min playfield_height
                    )
                    |> scroll_direction_transform bottom
                    |> _.AsQuad

                Render.tex_quad bounds tint.AsQuad (Sprite.pick_texture (animation.Loops, color) holdbody)

        let inline draw_tail_using_tail (key: int, tailpos: float32, headpos: float32, color: int, tint: Color) : unit =
            let clip = headpos + note_height * 0.5f
            let clip_percent = (clip - tailpos) / note_height

            let inline quad_clip_correction (quad: Quad) : Quad =
                if clip_percent > 0.0f then
                    let height = quad.BottomRight.Y - quad.TopRight.Y
                    let correction = OpenTK.Mathematics.Vector2(0.0f, height * clip_percent)

                    Quad.from_vectors(
                        quad.TopLeft + correction,
                        quad.TopRight + correction,
                        quad.BottomRight,
                        quad.BottomLeft
                    )
                else
                    quad

            let bounds =
                Rect.FromEdges(
                    left + column_positions.[key],
                    max clip tailpos,
                    left + column_positions.[key] + note_height,
                    tailpos + note_height
                )
                |> scroll_direction_transform bottom
                |> _.AsQuad
                |> hold_tail_transform key

            Render.tex_quad
                bounds
                tint.AsQuad
                (Sprite.pick_texture (animation.Loops, color) holdtail |> _.Transform(quad_clip_correction))

        let inline draw_tail_using_head (key: int, tailpos: float32, headpos: float32, color: int, tint: Color) : unit =
            let pos = max headpos tailpos

            let bounds =
                Rect.FromEdges(
                    left + column_positions.[key],
                    pos,
                    left + column_positions.[key] + note_height,
                    pos + note_height
                )
                |> scroll_direction_transform bottom
                |> _.AsQuad
                |> hold_tail_transform key

            Render.tex_quad bounds tint.AsQuad (Sprite.pick_texture (animation.Loops, color) holdhead)

        let inline draw_tail (key: int, tailpos: float32, headpos: float32, color: int, tint: Color) =
            if noteskin_config.UseHoldTailTexture then
                draw_tail_using_tail(key, tailpos, headpos, color, tint)
            else
                draw_tail_using_head(key, tailpos, headpos, color, tint)

        let inline prepare_hold_states () : unit =
            for key = 0 to keys - 1 do
                hold_draw_states.[key] <-
                    if holds_offscreen.[key] < 0 then NoHold else HeadOffscreen(holds_offscreen.[key])

        // RESULT: note_seek <- index of the next row to appear, or notes.Length if none left
        let inline seek_notes_to_time (time: Time) : unit =
            while note_seek < chart.Notes.Length && chart.Notes.[note_seek].Time < time do
                let nr = chart.Notes.[note_seek].Data

                for k = 0 to keys - 1 do
                    if nr.[k] = NoteType.HOLDHEAD then
                        holds_offscreen.[k] <- note_seek
                    elif nr.[k] = NoteType.HOLDTAIL then
                        holds_offscreen.[k] <- -1

                note_seek <- note_seek + 1

            prepare_hold_states()

        // RESULT: sv_seek <- index of the next sv to appear, or sv.Length if none left
        let inline seek_sv_to_time (time: Time) : unit =
            while sv_seek < sv.Length && sv.[sv_seek].Time < time do
                sv_seek <- sv_seek + 1

        let now =
            Song.time_with_offset()
            + (GameThread.frame_compensation() + options.VisualOffset.Value) * Song.playback_rate()

        if now < last_time then
            handle_seek_back_in_time()

        last_time <- now

        let begin_time =
            if vanishing_notes then
                let space_needed = hitposition + note_height
                let time_needed = space_needed / scroll_speed_scaled
                // todo: this is true at 1.0x SV but can be too small a margin for SV < 1.0x - maybe add a fade out effect cause im lazy
                now - time_needed
            else
                now

        seek_notes_to_time(begin_time)
        seek_sv_to_time(begin_time)

        let mutable note_peek = note_seek
        let mutable sv_peek = sv_seek

        let mutable sv_value = if sv_seek > 0 then sv.[sv_seek - 1].Data else 1.0f

        if sv_value < 0.0f then
            has_negative_sv <- true

        let inline calculate_starting_render_position () : float32 =
            let mutable starting_position = hitposition

            if vanishing_notes then
                let mutable i = sv_seek
                let mutable sv_v = sv_value
                let mutable sv_t = begin_time

                while i < sv.Length && sv.[i].Time < now do
                    let { Time = t2; Data = v } = sv.[i]
                    starting_position <- starting_position - scroll_speed_scaled * sv_v * (t2 - sv_t)
                    sv_t <- t2
                    sv_v <- v
                    i <- i + 1

                starting_position <- starting_position - scroll_speed_scaled * sv_v * (now - sv_t)

            starting_position

        let mutable current_render_position = calculate_starting_render_position()

        let mutable sv_time = begin_time
        let begin_pos = current_render_position

        let inline hold_tint (hold_state: HoldState) : Color =
            let dropped = hold_state = HoldState.Dropped || hold_state = HoldState.MissedHead
            if dropped then noteskin_config.DroppedHoldColor else Color.White

        let inline end_offscreen_ln_with_tail (index: int, key: int, color: int) : unit =
            let hold_state = state.Scoring.HoldState(index, key)

            if not vanishing_notes || hold_state <> HoldState.Released then

                let tint = hold_tint(hold_state)
                let head_and_body_color = int chart.Colors.[index].Data.[key]
                let headpos = if hold_state.ShowInReceptor then hitposition else begin_pos

                let tailpos =
                    let pos = current_render_position - holdnote_trim
                    let min_pos =
                        if noteskin_config.MinimumHoldNoteLength then headpos else headpos - note_height * 0.5f
                    max pos min_pos

                draw_body(key, headpos, tailpos, head_and_body_color, tint)
                draw_tail(key, tailpos, headpos, color, tint)

                if not vanishing_notes || hold_state.ShowInReceptor then
                    draw_head(key, headpos, head_and_body_color, tint)

                hold_draw_states.[key] <- NoHold

        let inline end_onscreen_ln_with_tail (index: int, key: int, headpos: float32, color: int) : unit =
            let hold_state = state.Scoring.HoldState(index, key)

            if not vanishing_notes || hold_state <> HoldState.Released then

                let tint = hold_tint(hold_state)
                let head_and_body_color = int chart.Colors.[index].Data.[key]
                let headpos = if hold_state.ShowInReceptor then max hitposition headpos else headpos

                let tailpos =
                    let pos = current_render_position - holdnote_trim
                    let min_pos =
                        if noteskin_config.MinimumHoldNoteLength then headpos else headpos - note_height * 0.5f
                    max pos min_pos

                draw_body(key, headpos, tailpos, head_and_body_color, tint)
                draw_tail(key, tailpos, headpos, color, tint)
                draw_head(key, headpos, head_and_body_color, tint)

                hold_draw_states.[key] <- NoHold

        let inline end_offscreen_ln (index: int, key: int) : unit =
            let hold_state = state.Scoring.HoldState(index, key)

            if not vanishing_notes || hold_state <> HoldState.Released then

                let tint = hold_tint(hold_state)
                let head_and_body_color = int chart.Colors.[index].Data.[key]
                let headpos = if hold_state.ShowInReceptor then hitposition else begin_pos
                let tailpos = bottom

                draw_body(key, headpos, tailpos, head_and_body_color, tint)

                if not vanishing_notes || hold_state.ShowInReceptor then
                    draw_head(key, headpos, head_and_body_color, tint)

        let inline end_onscreen_ln (index: int, key: int, headpos: float32) : unit =
            let hold_state = state.Scoring.HoldState(index, key)

            if not vanishing_notes || hold_state <> HoldState.Released then

                let tint = hold_tint(hold_state)
                let head_and_body_color = int chart.Colors.[index].Data.[key]
                let headpos = if hold_state.ShowInReceptor then max hitposition headpos else headpos
                let tailpos = bottom

                draw_body(key, headpos, tailpos, head_and_body_color, tint)
                draw_head(key, headpos, head_and_body_color, tint)

        let inline advance_sv_and_render_position (time: Time) : unit =
            while sv_peek < sv.Length && sv.[sv_peek].Time < time do
                let { Time = t2; Data = v } = sv.[sv_peek]
                current_render_position <- current_render_position + scroll_speed_scaled * sv_value * (t2 - sv_time)
                sv_time <- t2
                sv_value <- v

                if sv_value < 0.0f then
                    has_negative_sv <- true

                sv_peek <- sv_peek + 1

            current_render_position <- current_render_position + scroll_speed_scaled * sv_value * (time - sv_time)
            sv_time <- time

        let inline more_notes_to_draw () : bool =
            let not_at_end_of_chart = note_peek < chart.Notes.Length
            let not_yet_offscreen = current_render_position < playfield_height

            let negative_sv_buffer_remaining =
                has_negative_sv && note_peek - note_seek < NEGATIVE_SV_ROW_COUNT

            not_at_end_of_chart && (not_yet_offscreen || negative_sv_buffer_remaining)

        while more_notes_to_draw() do
            let next_row = chart.Notes.[note_peek]
            let colors = chart.Colors.[note_peek].Data
            advance_sv_and_render_position(next_row.Time)

            for key = 0 to keys - 1 do
                match next_row.Data.[key] with
                | NoteType.NORMAL ->
                    if not(vanishing_notes && state.Scoring.IsNoteHit(note_peek, key)) then
                        draw_note(key, current_render_position, int colors.[key])

                | NoteType.HOLDHEAD -> hold_draw_states.[key] <- HeadOnscreen(current_render_position, note_peek)

                | NoteType.HOLDTAIL ->
                    match hold_draw_states.[key] with
                    | HeadOffscreen index -> end_offscreen_ln_with_tail(index, key, int colors.[key])
                    | HeadOnscreen(headpos, index) -> end_onscreen_ln_with_tail(index, key, headpos, int colors.[key])
                    | NoHold -> assert false
                | _ -> ()

            note_peek <- note_peek + 1

        for key = 0 to keys - 1 do
            match hold_draw_states.[key] with
            | HeadOffscreen index -> end_offscreen_ln(index, key)
            | HeadOnscreen(headpos, index) -> end_onscreen_ln(index, key, headpos)
            | NoHold -> ()

    override this.Draw() : unit =

        this.DrawPlayfieldBackground()

        let hitposition = options.HitPosition.Value

        let lanecover_draw_under_receptors =
            options.LaneCover.Enabled.Value && options.LaneCover.DrawUnderReceptors.Value

        if not lanecover_draw_under_receptors then
            this.DrawJudgementLine(hitposition)

            if not noteskin_config.NotesUnderReceptors then
                this.DrawReceptors(hitposition)

        this.DrawNotes(hitposition)

        if lanecover_draw_under_receptors then
            Lanecover.draw(this.Bounds)
            this.DrawJudgementLine(hitposition)
            this.DrawReceptors(hitposition)

        elif noteskin_config.NotesUnderReceptors then
            this.DrawReceptors(hitposition)

        if noteskin_config.UseExplosions then
            explosions.Draw()

        base.Draw()
