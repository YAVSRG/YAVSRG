namespace Interlude.Features.Play

open System
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Audio
open Percyqaz.Flux.UI
open Prelude
open Prelude.Charts.Formats.Interlude
open Prelude.Charts.Tools.NoteColors
open Prelude.Data.Content
open Interlude
open Interlude.Options
open Interlude.Features

type Playfield(chart: ColorizedChart, state: PlayState) as this =
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

    // arrays of stuff that are reused/changed every frame. the data from the previous frame is not used, but making new arrays causes garbage collection
    let mutable note_seek = 0 // see comments for sv_seek and sv_peek. same role but for index of next row
    let mutable note_peek = note_seek
    let sv = chart.SV
    let mutable sv_seek = 0 // index of next appearing SV point; = number of SV points if there are no more
    let mutable sv_peek = sv_seek // same as sv_seek, but sv_seek stores the relevant point for t = now (according to music) and this stores t > now
    let mutable sv_value = 1.0f // value of the most recent sv point, equal to the sv value at index (sv_peek - 1), or 1 if that point doesn't exist
    let mutable sv_time = 0.0f<ms>
    let mutable column_pos = 0.0f // running position calculation of notes for sv
    let hold_presence = Array.create keys false
    let hold_pos = Array.create keys 0.0f
    let hold_colors = Array.create keys 0
    let hold_index = Array.create keys -1

    let rotation = Content.Noteskins.noteRotation keys

    let mutable time = -Time.infinity
    let reset() =
        note_seek <- 0
        sv_seek <- 0

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

    new (state: PlayState) = Playfield(Gameplay.Chart.colored(), state)

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

        let playfieldHeight = bottom - top + holdnoteTrim
        let now = Song.timeWithOffset() + options.VisualOffset.Value * 1.0f<ms> * Gameplay.rate.Value
        let begin_time = 
            if options.VanishingNotes.Value then 
                let space_needed = hitposition + noteHeight
                let time_needed = space_needed / scale
                now - time_needed // todo: this is true only with no SV but can be too small a margin for SV < 1.0x
            else now

        // variable setup before draw
        while note_seek < chart.Notes.Length && chart.Notes.[note_seek].Time < begin_time do
            let { Data = struct (nr, _) } = chart.Notes.[note_seek]
            for k = 0 to keys - 1 do
                if nr.[k] = NoteType.HOLDHEAD then hold_index.[k] <- note_seek
            note_seek <- note_seek + 1
        note_peek <- note_seek

        // move sv pointer
        while sv_seek < sv.Length && sv.[sv_seek].Time < begin_time do
            sv_seek <- sv_seek + 1
        sv_peek <- sv_seek
        sv_value <- if sv_seek > 0 then sv.[sv_seek - 1].Data else 1.0f

        // let's travel to now and see how far we went
        column_pos <- hitposition
        if options.VanishingNotes.Value then
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

        sv_time <- begin_time

        // draw column backdrops and receptors
        for k in 0 .. (keys - 1) do
            Draw.rect (Rect.Create(left + columnPositions.[k], top, left + columnPositions.[k] + column_width, bottom)) playfieldColor
            hold_pos.[k] <- hitposition
            hold_presence.[k] <-
                if note_seek > 0 then
                    let { Data = struct (nr, c) } = chart.Notes.[note_seek - 1] in
                    hold_colors.[k] <- int c.[k]
                    (nr.[k] = NoteType.HOLDHEAD || nr.[k] = NoteType.HOLDBODY)
                else false
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
                            pos_b + noteHeight * 0.5f)
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
                    |> rotation k
                )
                (Quad.colorOf tint)
                (Sprite.gridUV (animation.Loops, color) (Content.getTexture "holdtail") |> fun struct (s, q) -> struct (s, scrollDirectionFlip q))

        // main render loop - until the last note rendered in every column appears off screen
        while column_pos < playfieldHeight && note_peek < chart.Notes.Length do
            let { Time = t; Data = struct (nd, color) } = chart.Notes.[note_peek]
            // update sv
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
                if nd.[k] = NoteType.NORMAL && not (options.VanishingNotes.Value && state.Scoring.IsNoteHit note_peek k) then
                    draw_note(k, column_pos, int color.[k])

                elif nd.[k] = NoteType.HOLDHEAD then
                    hold_pos.[k] <- max column_pos hitposition
                    hold_colors.[k] <- int color.[k]
                    hold_presence.[k] <- true

                elif nd.[k] = NoteType.HOLDTAIL then
                    let headpos = hold_pos.[k]
                    let tint = if hold_pos.[k] = hitposition && state.Scoring.IsHoldDropped hold_index.[k] k then Content.noteskinConfig().DroppedHoldColor else Color.White
                    let pos = column_pos - holdnoteTrim
                    if headpos < pos then
                        draw_body(k, headpos, pos, hold_colors.[k], tint)
                    if headpos - pos < noteHeight * 0.5f then
                        draw_tail(k, pos, headpos, int color.[k], tint)
                    draw_head(k, headpos, hold_colors.[k], tint)
                    hold_presence.[k] <- false
            note_peek <- note_peek + 1
        
        for k in 0 .. (keys - 1) do
            if hold_presence.[k] then
                let headpos = hold_pos.[k]
                let tint = if hold_pos.[k] = hitposition && state.Scoring.IsHoldDropped hold_index.[k] k then Content.noteskinConfig().DroppedHoldColor else Color.White
                draw_body(k, headpos, bottom, hold_colors.[k], tint)
                draw_head(k, headpos, hold_colors.[k], tint)

        base.Draw()