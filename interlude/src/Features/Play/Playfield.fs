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

    override this.Draw() =
        let width = sprite.AspectRatio * this.Bounds.Height
        Render.sprite (Rect.FromSize(this.Bounds.Left - width, this.Bounds.Top, width, this.Bounds.Height)) Color.White sprite

type private StageRight() =
    inherit StaticWidget(NodeType.None)

    let sprite = Content.Texture "stageright"

    override this.Draw() =
        let width = sprite.AspectRatio * this.Bounds.Height
        Render.sprite (Rect.FromSize(this.Bounds.Right, this.Bounds.Top, width, this.Bounds.Height)) Color.White sprite

[<Struct>]
type private HoldRenderState =
    | HeadOffscreen of int
    | HeadOnscreen of pos: float32 * index: int
    | NoHold

type Playfield(chart: ColoredChart, state: PlayState, noteskin_config: NoteskinConfig, vanishing_notes: bool) as this =
    inherit Container(NodeType.None)

    let NEGATIVE_SV_ROW_COUNT = 150

    let keys = chart.Keys

    let column_width = noteskin_config.KeymodeColumnWidth keys
    let column_spacing = noteskin_config.KeymodeColumnSpacing keys

    let column_positions =
        let mutable x = 0.0f

        Array.init
            keys
            (fun i ->
                let v = x

                if i + 1 < keys then
                    x <- x + column_width + column_spacing.[i]

                v
            )

    let column_lighting = ColumnLighting(chart.Keys, noteskin_config, state)
    let explosions = Explosions(chart.Keys, noteskin_config, state)

    let note_height = column_width
    let holdnote_trim = column_width * noteskin_config.HoldNoteTrim
    let playfield_color = noteskin_config.PlayfieldColor
    let fill_column_gaps = noteskin_config.FillColumnGaps
    let receptor_colors = noteskin_config.ReceptorColors.[keys - 3]

    let receptor = Content.Texture "receptor"
    let judgement_line = Content.Texture "judgementline"
    let holdtail = Content.Texture "holdtail"
    let holdhead = Content.Texture "holdhead"
    let holdbody = Content.Texture "holdbody"
    let note = Content.Texture "note"
    let animation = Animation.Counter(float noteskin_config.AnimationFrameTime)

    let sv = chart.SV
    let mutable note_seek = 0
    let mutable sv_seek = 0
    let holds_offscreen = Array.create keys -1
    let hold_states = Array.create keys NoHold

    let rotation : int -> Quad -> Quad =
        if noteskin_config.UseRotation then
            let rotations = noteskin_config.Rotations.[keys - 3]
            fun k -> Quad.rotate (rotations.[k])
        else
            fun _ quad -> quad

    let mutable has_negative_sv = false
    let mutable time = -Time.infinity

    let handle_seek_back_in_time () : unit =
        note_seek <- 0
        sv_seek <- 0

        for k = 0 to hold_states.Length - 1 do
            hold_states.[k] <- NoHold
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
        if not noteskin_config.UseHoldTailTexture then rotation k
        elif not noteskin_config.FlipHoldTail || options.Upscroll.Value then
            id
        else
            Quad.flip_vertical

    let receptor_transform (k: int) : Quad -> Quad =
        if noteskin_config.ReceptorStyle = ReceptorStyle.Flip then
            if options.Upscroll.Value then Quad.flip_vertical else id
        else
            rotation k

    let judgement_line_transform : Quad -> Quad =
        if options.Upscroll.Value then Quad.flip_vertical else id

    do
        let width = Array.mapi (fun i n -> n + column_width) column_positions |> Array.max

        let (screen_align_percentage, playfield_align_percentage) =
            noteskin_config.PlayfieldAlignment

        if noteskin_config.EnableStageTextures then
            this |+ StageLeft() |* StageRight()

        this.Position <-
            {
                Left = screen_align_percentage %- (width * playfield_align_percentage)
                Top = Position.MIN
                Right = screen_align_percentage %+ (width * (1.0f - playfield_align_percentage))
                Bottom = Position.MAX
            }

    member this.ColumnWidth = column_width
    member this.ColumnPositions = column_positions

    override this.Init(parent) =
        base.Init parent

        if noteskin_config.EnableColumnLight then
            column_lighting.Init this

        if noteskin_config.UseExplosions then
            explosions.Init this

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        animation.Update elapsed_ms

        if noteskin_config.EnableColumnLight then
            column_lighting.Update(elapsed_ms, moved)

        if noteskin_config.UseExplosions then
            explosions.Update(elapsed_ms, moved)

    override this.Draw() =
        let {
                Rect.Left = left
                Top = top
                Right = right
                Bottom = bottom
            } =
            this.Bounds

        // SETUP CONSTANTS AND DRAW METHODS

        let scale = options.ScrollSpeed.Value / SelectedChart.rate.Value
        let hitposition = float32 options.HitPosition.Value

        let playfield_height = bottom - top + (max 0.0f holdnote_trim)
        let receptor_aspect_ratio = receptor.AspectRatio

        let inline draw_judgement_line () : unit =
            if noteskin_config.UseJudgementLine then
                let area =
                    Rect.FromEdges(
                        left,
                        hitposition + (note_height - note_height * noteskin_config.JudgementLineScale) * 0.5f,
                        right,
                        hitposition + (note_height + note_height * noteskin_config.JudgementLineScale) * 0.5f
                    ).TranslateY(note_height * noteskin_config.JudgementLineOffset)
                    |> scroll_direction_transform bottom
                    |> _.AsQuad
                    |> judgement_line_transform
                Render.tex_quad
                    area
                    Color.White.AsQuad
                    (Sprite.pick_texture (animation.Loops, 0) judgement_line)

        let inline draw_receptors () : unit =
            if noteskin_config.UseReceptors then
                for k in 0 .. (keys - 1) do
                    Render.tex_quad
                        (Rect.FromSize(
                            left + column_positions.[k],
                            hitposition + note_height - note_height / receptor_aspect_ratio,
                            column_width,
                            note_height / receptor_aspect_ratio
                         ).TranslateY(note_height * noteskin_config.ReceptorOffset)
                         |> scroll_direction_transform bottom
                         |> _.AsQuad
                         |> receptor_transform k)
                        Color.White.AsQuad
                        (Sprite.pick_texture
                            (animation.Loops,
                            receptor_colors.[k] * 2 +
                             if (state.Scoring.KeyState |> Bitmask.has_key k) then
                                 1
                             else
                                 0)
                            receptor)

            if noteskin_config.EnableColumnLight then
                column_lighting.Draw()

        let inline draw_note (k: int, pos: float32, color: int) : unit =
            Render.tex_quad
                ((Rect.FromSize(left + column_positions.[k], pos, column_width, note_height)
                  |> scroll_direction_transform bottom)
                    .AsQuad
                 |> rotation k)
                Color.White.AsQuad
                (Sprite.pick_texture (animation.Loops, color) note)

        let inline draw_head (k: int, pos: float32, color: int, tint: Color) : unit =
            Render.tex_quad
                ((Rect.FromSize(left + column_positions.[k], pos, column_width, note_height)
                  |> scroll_direction_transform bottom)
                    .AsQuad
                 |> rotation k)
                tint.AsQuad
                (Sprite.pick_texture (animation.Loops, color) holdhead)

        let inline draw_body (k: int, pos_a: float32, pos_b: float32, color: int, tint: Color) : unit =
            Render.tex_quad
                ((Rect.FromEdges(
                    left + column_positions.[k],
                    pos_a + note_height * 0.5f,
                    left + column_positions.[k] + column_width,
                    pos_b + note_height * 0.5f + 2.0f |> min playfield_height
                  )
                  |> scroll_direction_transform bottom)
                    .AsQuad)
                tint.AsQuad
                (Sprite.pick_texture (animation.Loops, color) holdbody)

        let inline draw_tail_using_tail (k: int, pos: float32, clip: float32, color: int, tint: Color) : unit =
            let clip_percent = (clip - pos) / note_height

            let quad_clip_correction (q: Quad) : Quad =
                if clip_percent > 0.0f then
                    let height = q.BottomRight.Y - q.TopRight.Y
                    let correction = OpenTK.Mathematics.Vector2(0.0f, height * clip_percent)
                    Quad.from_vectors(q.TopLeft + correction, q.TopRight + correction, q.BottomRight, q.BottomLeft)
                else q

            Render.tex_quad
                (
                    (
                        Rect.FromEdges(
                            left + column_positions.[k],
                            max clip pos,
                            left + column_positions.[k] + note_height,
                            pos + note_height
                        )
                        |> scroll_direction_transform bottom
                    ).AsQuad
                    |> hold_tail_transform k
                )
                tint.AsQuad
                (
                    Sprite.pick_texture
                        (animation.Loops, color)
                        holdtail
                    |> fun x -> x.Transform(quad_clip_correction)
                )

        let inline draw_tail_using_head (k: int, pos: float32, clip: float32, color: int, tint: Color) : unit =
            let pos = max (clip - note_height * 0.5f) pos

            Render.tex_quad
                (
                    (
                        Rect.FromEdges(
                            left + column_positions.[k],
                            pos,
                            left + column_positions.[k] + note_height,
                            pos + note_height
                        )
                        |> scroll_direction_transform bottom
                    ).AsQuad
                    |> hold_tail_transform k
                )
                tint.AsQuad
                (
                    Sprite.pick_texture
                        (animation.Loops, color)
                        holdhead
                )

        let draw_tail = if noteskin_config.UseHoldTailTexture then draw_tail_using_tail else draw_tail_using_head

        // CALCULATE TIME + SV STUFF

        let now =
            Song.time_with_offset () +
            (GameThread.frame_compensation () + options.VisualOffset.Value) * Song.playback_rate()

        if now < time then
            handle_seek_back_in_time ()

        time <- now

        let begin_time =
            if vanishing_notes then
                let space_needed = hitposition + note_height
                let time_needed = space_needed / scale
                now - time_needed // todo: this is true at 1.0x SV but can be too small a margin for SV < 1.0x - maybe add a fade out effect cause im lazy
            else
                now

        // note_seek = index of the next row to appear, or notes.Length if none left
        while note_seek < chart.Notes.Length && chart.Notes.[note_seek].Time < begin_time do
            let nr = chart.Notes.[note_seek].Data

            for k = 0 to keys - 1 do
                if nr.[k] = NoteType.HOLDHEAD then
                    holds_offscreen.[k] <- note_seek
                elif nr.[k] = NoteType.HOLDTAIL then
                    holds_offscreen.[k] <- -1

            note_seek <- note_seek + 1

        let mutable note_peek = note_seek

        // sv_seek = index of the next sv to appear, or sv.Length if none left
        while sv_seek < sv.Length && sv.[sv_seek].Time < begin_time do
            sv_seek <- sv_seek + 1

        let mutable sv_value = if sv_seek > 0 then sv.[sv_seek - 1].Data else 1.0f
        if sv_value < 0.0f then has_negative_sv <- true
        let mutable sv_peek = sv_seek

        // calculation of where to start drawing from (for vanishing notes this depends on sv between begin_time and now)
        let mutable column_pos = hitposition

        if vanishing_notes then
            let mutable i = sv_seek
            let mutable sv_v = sv_value
            let mutable sv_t = begin_time

            while (i < sv.Length && sv.[i].Time < now) do
                let { Time = t2; Data = v } = sv.[i]
                column_pos <- column_pos - scale * sv_v * (t2 - sv_t)
                sv_t <- t2
                sv_v <- v
                i <- i + 1

            column_pos <- column_pos - scale * sv_v * (now - sv_t)

        let mutable sv_time = begin_time
        let begin_pos = column_pos

        // ACTUAL DRAWING STUFF

        // draw playfield color
        if fill_column_gaps then
            Render.rect this.Bounds playfield_color

        for k in 0 .. (keys - 1) do
            if not fill_column_gaps then
                Render.rect_edges
                    (left + column_positions.[k])
                    top
                    (left + column_positions.[k] + column_width)
                    bottom
                    playfield_color

            hold_states.[k] <-
                if holds_offscreen.[k] < 0 then
                    NoHold
                else
                    HeadOffscreen holds_offscreen.[k]

        if not (options.LaneCover.Enabled.Value && options.LaneCover.DrawUnderReceptors.Value) then
            draw_judgement_line()
            if not noteskin_config.NotesUnderReceptors then
                draw_receptors()

        // main render loop - draw notes at column_pos until you go offscreen, column_pos increases* with every row drawn
        while (column_pos < playfield_height || (has_negative_sv && note_peek - note_seek < NEGATIVE_SV_ROW_COUNT)) && note_peek < chart.Notes.Length do

            let { Time = t; Data = nr } = chart.Notes.[note_peek]
            let color = chart.Colors.[note_peek].Data
            // update vertical position + scroll speed based on sv
            while (sv_peek < sv.Length && sv.[sv_peek].Time < t) do
                let { Time = t2; Data = v } = sv.[sv_peek]
                column_pos <- column_pos + scale * sv_value * (t2 - sv_time)
                sv_time <- t2
                sv_value <- v
                if sv_value < 0.0f then has_negative_sv <- true
                sv_peek <- sv_peek + 1

            // render notes
            column_pos <- column_pos + scale * sv_value * (t - sv_time)
            sv_time <- t

            for k in 0 .. (keys - 1) do
                if
                    nr.[k] = NoteType.NORMAL
                    && not (vanishing_notes && state.Scoring.IsNoteHit note_peek k)
                then
                    draw_note (k, column_pos, int color.[k])

                elif nr.[k] = NoteType.HOLDHEAD then
                    // assert hold_states.[k] = NoHold
                    hold_states.[k] <- HeadOnscreen(column_pos, note_peek)

                elif nr.[k] = NoteType.HOLDTAIL then
                    match hold_states.[k] with
                    | HeadOffscreen i ->
                        let hold_state = state.Scoring.HoldState i k

                        if vanishing_notes && hold_state = HoldState.Released then
                            ()
                        else

                            let tint =
                                if hold_state = HoldState.Dropped || hold_state = HoldState.MissedHead then
                                    noteskin_config.DroppedHoldColor
                                else
                                    Color.White

                            let tailpos = column_pos - holdnote_trim
                            let headpos = if hold_state.ShowInReceptor then hitposition else begin_pos

                            let head_and_body_color = let colors = chart.Colors.[i].Data in int colors.[k]

                            if headpos < tailpos then
                                draw_body (k, headpos, tailpos, head_and_body_color, tint)

                            if headpos - tailpos < note_height * 0.5f then
                                draw_tail (k, tailpos, headpos + note_height * 0.5f, int color.[k], tint)

                            if not vanishing_notes || hold_state.ShowInReceptor then
                                draw_head (k, headpos, head_and_body_color, tint)

                            hold_states.[k] <- NoHold

                    | HeadOnscreen(headpos, i) ->
                        let hold_state = state.Scoring.HoldState i k

                        if vanishing_notes && hold_state = HoldState.Released then
                            ()
                        else

                            let tint =
                                if hold_state = HoldState.Dropped || hold_state = HoldState.MissedHead then
                                    noteskin_config.DroppedHoldColor
                                else
                                    Color.White

                            let tailpos = column_pos - holdnote_trim

                            let headpos =
                                if hold_state.ShowInReceptor then
                                    max hitposition headpos
                                else
                                    headpos

                            let head_and_body_color = let colors = chart.Colors.[i].Data in int colors.[k]

                            if headpos < tailpos then
                                draw_body (k, headpos, tailpos, head_and_body_color, tint)

                            if headpos - tailpos < note_height * 0.5f then
                                draw_tail (k, tailpos, headpos + note_height * 0.5f, int color.[k], tint)

                            draw_head (k, headpos, head_and_body_color, tint)

                            hold_states.[k] <- NoHold
                    | _ -> () // assert impossible

            note_peek <- note_peek + 1

        for k in 0 .. (keys - 1) do
            match hold_states.[k] with
            | HeadOffscreen i ->
                let hold_state = state.Scoring.HoldState i k

                if vanishing_notes && hold_state = HoldState.Released then
                    ()
                else

                    let tint =
                        if hold_state = HoldState.Dropped || hold_state = HoldState.MissedHead then
                            noteskin_config.DroppedHoldColor
                        else
                            Color.White

                    let tailpos = bottom
                    let headpos = if hold_state.ShowInReceptor then hitposition else begin_pos

                    let head_and_body_color = let colors = chart.Colors.[i].Data in int colors.[k]

                    if headpos < tailpos then
                        draw_body (k, headpos, tailpos, head_and_body_color, tint)

                    if not vanishing_notes || hold_state.ShowInReceptor then
                        draw_head (k, headpos, head_and_body_color, tint)

                    hold_states.[k] <- NoHold

            | HeadOnscreen(headpos, i) ->
                let hold_state = state.Scoring.HoldState i k

                if vanishing_notes && hold_state = HoldState.Released then
                    ()
                else

                    let tint =
                        if hold_state = HoldState.Dropped || hold_state = HoldState.MissedHead then
                            noteskin_config.DroppedHoldColor
                        else
                            Color.White

                    let tailpos = bottom

                    let headpos =
                        if hold_state.ShowInReceptor then
                            max hitposition headpos
                        else
                            headpos

                    let head_and_body_color = let colors = chart.Colors.[i].Data in int colors.[k]

                    if headpos < tailpos then
                        draw_body (k, headpos, tailpos, head_and_body_color, tint)

                    draw_head (k, headpos, head_and_body_color, tint)

                    hold_states.[k] <- NoHold
            | NoHold -> ()

        if options.LaneCover.Enabled.Value && options.LaneCover.DrawUnderReceptors.Value then
            Lanecover.draw(this.Bounds)
            draw_judgement_line()
            draw_receptors()
        elif noteskin_config.NotesUnderReceptors then
            draw_receptors()

        if noteskin_config.UseExplosions then
            explosions.Draw()
        base.Draw()