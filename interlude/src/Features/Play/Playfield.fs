namespace Interlude.Features.Play

open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Audio
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay
open Prelude.Charts.Formats.Interlude
open Prelude.Charts.Tools.NoteColors
open Prelude.Data.Content
open Interlude
open Interlude.Options
open Interlude.Features

[<Struct>]
type private HoldRenderState =
    | HeadOffscreen of int
    | HeadOnscreen of pos: float32 * index: int
    | NoHold

type Playfield(chart: ColorizedChart, state: PlayState, vanishing_notes) as this =
    inherit StaticContainer(NodeType.None)

    let keys = chart.Keys

    let column_width = Content.noteskinConfig().ColumnWidth
    let column_spacing = Content.noteskinConfig().ColumnSpacing

    let columnPositions = Array.init keys (fun i -> float32 i * (column_width + column_spacing))
    let noteHeight = column_width
    let holdnoteTrim = column_width * Content.noteskinConfig().HoldNoteTrim
    let playfieldColor = Content.noteskinConfig().PlayfieldColor

    let tailsprite = Content.getTexture(if Content.noteskinConfig().UseHoldTailTexture then "holdtail" else "holdhead")
    let animation = Animation.Counter (Content.noteskinConfig().AnimationFrameTime)

    let sv = chart.SV
    let mutable note_seek = 0
    let mutable sv_seek = 0
    let holds_offscreen = Array.create keys -1
    let hold_states = Array.create keys NoHold

    let rotation = Content.Noteskins.noteRotation keys

    let mutable time = -Time.infinity
    let reset() =
        note_seek <- 0
        sv_seek <- 0
        for k = 0 to hold_states.Length - 1 do
            hold_states.[k] <- NoHold

    let scrollDirectionPos bottom : Rect -> Rect =
        if options.Upscroll.Value then id 
        else fun (r: Rect) -> { Left = r.Left; Top = bottom - r.Bottom; Right = r.Right; Bottom = bottom - r.Top }
    let scrollDirectionFlip = fun q -> if not (Content.noteskinConfig().FlipHoldTail) || options.Upscroll.Value then q else Quad.flip q

    do
        let width = Array.mapi (fun i n -> n + column_width) columnPositions |> Array.max
        let (screenAlign, columnAlign) = Content.noteskinConfig().PlayfieldAlignment
        this.Position <-
            { 
                Left = screenAlign %- (width * columnAlign)
                Top = Position.min
                Right = screenAlign %+ (width * (1.0f - columnAlign))
                Bottom = Position.max
            }

    new (state: PlayState, vanishing_notes) = Playfield(Gameplay.Chart.colored(), state, vanishing_notes)

    member this.ColumnWidth = column_width
    member this.ColumnPositions = columnPositions

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        animation.Update elapsedTime
        let newtime = Song.time()
        if newtime < time then reset()
        time <- newtime

    override this.Draw() =
        let { Rect.Left = left; Top = top; Right = right; Bottom = bottom } = this.Bounds

        let scale = options.ScrollSpeed.Value / Gameplay.rate.Value * 1.0f</ms>
        let hitposition = float32 options.HitPosition.Value

        let playfieldHeight = bottom - top + (max 0.0f holdnoteTrim)
        let now = Song.timeWithOffset() + float32 Render.Performance.visual_latency * 1.0f<ms> + options.VisualOffset.Value * 1.0f<ms> * Gameplay.rate.Value
        let begin_time = 
            if vanishing_notes then 
                let space_needed = hitposition + noteHeight
                let time_needed = space_needed / scale
                now - time_needed // todo: this is true at 1.0x SV but can be too small a margin for SV < 1.0x - maybe add a fade out effect cause im a laze
            else now

        // note_seek = index of the next row to appear, or notes.Length if none left
        while note_seek < chart.Notes.Length && chart.Notes.[note_seek].Time < begin_time do
            let { Data = struct (nr, _) } = chart.Notes.[note_seek]
            for k = 0 to keys - 1 do
                if nr.[k] = NoteType.HOLDHEAD then holds_offscreen.[k] <- note_seek
                elif nr.[k] = NoteType.HOLDTAIL then holds_offscreen.[k] <- -1
            note_seek <- note_seek + 1
        let mutable note_peek = note_seek

        // sv_seek = index of the next sv to appear, or sv.Length if none left
        while sv_seek < sv.Length && sv.[sv_seek].Time < begin_time do
            sv_seek <- sv_seek + 1
        let mutable sv_value = if sv_seek > 0 then sv.[sv_seek - 1].Data else 1.0f
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

        // draw column backdrops and receptors
        for k in 0 .. (keys - 1) do
            Draw.rect (Rect.Create(left + columnPositions.[k], top, left + columnPositions.[k] + column_width, bottom)) playfieldColor
            hold_states.[k] <- if holds_offscreen.[k] < 0 then NoHold else HeadOffscreen holds_offscreen.[k]
            Draw.quad // receptor
                (
                    Rect.Box(left + columnPositions.[k], hitposition, column_width, noteHeight)
                    |> scrollDirectionPos bottom
                    |> Quad.ofRect
                    |> rotation k
                )
                (Color.White |> Quad.colorOf)
                (Sprite.gridUV (animation.Loops, if (state.Scoring.KeyState |> Bitmask.hasBit k) then 1 else 0) (Content.getTexture "receptor"))

        let inline draw_note(k, pos, color) =
            Draw.quad
                (
                    Quad.ofRect ( 
                        Rect.Box(left + columnPositions.[k], pos, column_width, noteHeight)
                        |> scrollDirectionPos bottom
                    )
                    |> rotation k
                )
                (Quad.colorOf Color.White)
                (Sprite.gridUV (animation.Loops, color) (Content.getTexture "note"))

        let inline draw_head(k, pos, color, tint) =
            Draw.quad
                (
                    Quad.ofRect ( 
                        Rect.Box(left + columnPositions.[k], pos, column_width, noteHeight)
                        |> scrollDirectionPos bottom
                    )
                    |> rotation k
                )
                (Quad.colorOf tint)
                (Sprite.gridUV (animation.Loops, color) (Content.getTexture "holdhead"))

        let inline draw_body(k, pos_a, pos_b, color, tint) =
            Draw.quad
                (
                    Quad.ofRect ( 
                        Rect.Create(
                            left + columnPositions.[k],
                            pos_a + noteHeight * 0.5f,
                            left + columnPositions.[k] + column_width,
                            pos_b + noteHeight * 0.5f + 2.0f)
                        |> scrollDirectionPos bottom
                    )
                )
                (Quad.colorOf tint)
                (Sprite.gridUV (animation.Loops, color) (Content.getTexture "holdbody"))

        let inline draw_tail(k, pos, clip, color, tint) =
            Draw.quad
                (
                    Quad.ofRect ( 
                        Rect.Create(left + columnPositions.[k], max clip pos, left + columnPositions.[k] + column_width, pos + noteHeight)
                        |> scrollDirectionPos bottom
                    )
                )
                (Quad.colorOf tint)
                (Sprite.gridUV (animation.Loops, color) tailsprite |> fun struct (s, q) -> struct (s, scrollDirectionFlip q))

        // main render loop - draw notes at column_pos until you go offscreen, column_pos increases* with every row drawn
        // todo: also put a cap at -playfieldHeight when *negative sv comes into play
        while column_pos < playfieldHeight && note_peek < chart.Notes.Length do

            let { Time = t; Data = struct (nd, color) } = chart.Notes.[note_peek]
            // update vertical position + scroll speed based on sv
            while (sv_peek < sv.Length && sv.[sv_peek].Time < t) do
                let { Time = t2; Data = v } = sv.[sv_peek]
                column_pos <- column_pos + scale * sv_value * (t2 - sv_time)
                sv_time <- t2
                sv_value <- v
                sv_peek <- sv_peek + 1

            // render notes
            column_pos <- column_pos + scale * sv_value * (t - sv_time)
            sv_time <- t

            for k in 0 .. (keys - 1) do
                if nd.[k] = NoteType.NORMAL && not (vanishing_notes && state.Scoring.IsNoteHit note_peek k) then
                    draw_note(k, column_pos, int color.[k])

                elif nd.[k] = NoteType.HOLDHEAD then
                    // assert hold_states.[k] = NoHold
                    hold_states.[k] <- HeadOnscreen (column_pos, note_peek)

                elif nd.[k] = NoteType.HOLDTAIL then
                    match hold_states.[k] with
                    | HeadOffscreen i ->
                        let hold_state = state.Scoring.HoldState i k
                        if vanishing_notes && hold_state = HoldState.Released then () else

                        let tint = if hold_state = HoldState.Dropped || hold_state = HoldState.MissedHead then Content.noteskinConfig().DroppedHoldColor else Color.White
                        let tailpos = column_pos - holdnoteTrim
                        let headpos = if hold_state.ShowInReceptor then hitposition else begin_pos
                        let head_and_body_color = let { Data = struct (_, colors) } = chart.Notes.[i] in int colors.[k]

                        if headpos < tailpos then
                            draw_body(k, headpos, tailpos, head_and_body_color, tint)
                        if headpos - tailpos < noteHeight * 0.5f then
                            draw_tail(k, tailpos, headpos, int color.[k], tint)
                        if not vanishing_notes || hold_state.ShowInReceptor then
                            draw_head(k, headpos, head_and_body_color, tint)
                            
                        hold_states.[k] <- NoHold

                    | HeadOnscreen (headpos, i) ->
                        let hold_state = state.Scoring.HoldState i k
                        if vanishing_notes && hold_state = HoldState.Released then () else
                        
                        let tint = if hold_state = HoldState.Dropped || hold_state = HoldState.MissedHead then Content.noteskinConfig().DroppedHoldColor else Color.White
                        let tailpos = column_pos - holdnoteTrim
                        let headpos = if hold_state.ShowInReceptor then max hitposition headpos else headpos
                        let head_and_body_color = let { Data = struct (_, colors) } = chart.Notes.[i] in int colors.[k]
                        
                        if headpos < tailpos then
                            draw_body(k, headpos, tailpos, head_and_body_color, tint)
                        if headpos - tailpos < noteHeight * 0.5f then
                            draw_tail(k, tailpos, headpos, int color.[k], tint)
                        draw_head(k, headpos, head_and_body_color, tint)

                        hold_states.[k] <- NoHold
                    | _ -> () // assert impossible
            note_peek <- note_peek + 1
        
        for k in 0 .. (keys - 1) do
            match hold_states.[k] with
            | HeadOffscreen i ->
                let hold_state = state.Scoring.HoldState i k
                if vanishing_notes && hold_state = HoldState.Released then () else

                let tint = if hold_state = HoldState.Dropped || hold_state = HoldState.MissedHead then Content.noteskinConfig().DroppedHoldColor else Color.White
                let tailpos = bottom
                let headpos = if hold_state.ShowInReceptor then hitposition else begin_pos
                let head_and_body_color = let { Data = struct (_, colors) } = chart.Notes.[i] in int colors.[k]

                if headpos < tailpos then draw_body(k, headpos, tailpos, head_and_body_color, tint)
                if not vanishing_notes || hold_state.ShowInReceptor then
                    draw_head(k, headpos, head_and_body_color, tint)

                hold_states.[k] <- NoHold

            | HeadOnscreen (headpos, i) ->
                let hold_state = state.Scoring.HoldState i k
                if vanishing_notes && hold_state = HoldState.Released then () else
                
                let tint = if hold_state = HoldState.Dropped || hold_state = HoldState.MissedHead then Content.noteskinConfig().DroppedHoldColor else Color.White
                let tailpos = bottom
                let headpos = if hold_state.ShowInReceptor then max hitposition headpos else headpos
                let head_and_body_color = let { Data = struct (_, colors) } = chart.Notes.[i] in int colors.[k]
                
                if headpos < tailpos then
                    draw_body(k, headpos, tailpos, head_and_body_color, tint)
                draw_head(k, headpos, head_and_body_color, tint)
                
                hold_states.[k] <- NoHold
            | NoHold -> ()

        base.Draw()