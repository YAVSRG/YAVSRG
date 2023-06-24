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

// TODO LIST
//  COLUMN INDEPENDENT SV
//  FIX HOLD TAIL CLIPPING

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
    let sv = Array.init (keys + 1) (fun i -> if i = 0 then chart.SV else chart.ColumnSV.[i - 1])
    let sv_seek = Array.create (keys + 1) 0 // index of next appearing SV point for each channel; = number of SV points if there are no more
    let sv_peek = Array.create (keys + 1) 0 // same as sv_seek, but sv_seek stores the relevant point for t = now (according to music) and this stores t > now
    let sv_value = Array.create (keys + 1) 1.0f // value of the most recent sv point per channel, equal to the sv value at index (sv_peek - 1), or 1 if that point doesn't exist
    let sv_time = Array.zeroCreate (keys + 1)
    let column_pos = Array.zeroCreate keys // running position calculation of notes for sv
    let hold_presence = Array.create keys false
    let hold_pos = Array.create keys 0.0f
    let hold_colors = Array.create keys 0
    let hold_index = Array.create keys -1

    let rotation = Content.Noteskins.noteRotation keys

    let mutable time = -Time.infinity
    let reset() =
        note_seek <- 0
        for k = 0 to keys do sv_seek.[k] <- 0

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
        let begin_time = if options.VanishingNotes.Value then now - state.Ruleset.Accuracy.MissWindow * Gameplay.rate.Value else now

        // seek to appropriate sv and note locations in data.
        // bit of a mess here. see comments on the variables for more on whats going on
        while note_seek < chart.Notes.Length && chart.Notes.[note_seek].Time < begin_time do
            let { Data = struct (nr, _) } = chart.Notes.[note_seek]
            for k = 0 to keys - 1 do
                if nr.[k] = NoteType.HOLDHEAD then hold_index.[k] <- note_seek
            note_seek <- note_seek + 1
        note_peek <- note_seek
        for i = 0 to keys do
            while sv_seek.[i] < sv.[i].Length && sv.[i].[sv_seek.[i]].Time < now do
                sv_seek.[i] <- sv_seek.[i] + 1
            sv_peek.[i] <- sv_seek.[i]
            sv_value.[i] <- if sv_seek.[i] > 0 then sv.[i].[sv_seek.[i] - 1].Data else 1.0f

        for k in 0 .. (keys - 1) do
            Draw.rect (Rect.Create(left + columnPositions.[k], top, left + columnPositions.[k] + column_width, bottom)) playfieldColor
            sv_time.[k] <- now
            column_pos.[k] <- hitposition
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

        // main render loop - until the last note rendered in every column appears off screen
        let mutable min = hitposition
        while min < playfieldHeight && note_peek < chart.Notes.Length do
            min <- playfieldHeight
            let { Time = t; Data = struct (nd, color) } = chart.Notes.[note_peek]
            // until no sv adjustments needed...
            // update main sv
            while (sv_peek.[0] < sv.[0].Length && sv.[0].[sv_peek.[0]].Time < t) do
                let { Time = t2; Data = v } = sv.[0].[sv_peek.[0]]
                for k in 0 .. (keys - 1) do
                    column_pos.[k] <- column_pos.[k] + scale * sv_value.[0] * (t2 - sv_time.[k])
                    sv_time.[k] <- t2
                sv_value.[0] <- v
                sv_peek.[0] <- sv_peek.[0] + 1
            // todo: updating column sv goes here

            // render notes
            for k in 0 .. (keys - 1) do
                column_pos.[k] <- column_pos.[k] + scale * sv_value.[0] * (t - sv_time.[k])
                sv_time.[k] <- t
                min <- Math.Min(column_pos.[k], min)
                if nd.[k] = NoteType.NORMAL && not (options.VanishingNotes.Value && state.Scoring.IsNoteHit note_peek k) then
                    Draw.quad // normal note
                        (
                            Quad.ofRect ( 
                                Rect.Box(left + columnPositions.[k], column_pos.[k], column_width, noteHeight)
                                |> scrollDirectionPos bottom
                            )
                            |> rotation k
                        )
                        (Quad.colorOf Color.White)
                        (Sprite.gridUV (animation.Loops, int color.[k]) (Content.getTexture "note"))
                elif nd.[k] = NoteType.HOLDHEAD then
                    hold_pos.[k] <- max column_pos.[k] hitposition
                    hold_colors.[k] <- int color.[k]
                    hold_presence.[k] <- true
                elif nd.[k] = NoteType.HOLDTAIL then
                    let headpos = hold_pos.[k]
                    let tint = if hold_pos.[k] = hitposition && state.Scoring.IsHoldDropped hold_index.[k] k then Content.noteskinConfig().DroppedHoldColor else Color.White
                    let pos = column_pos.[k] - holdnoteTrim
                    if headpos < pos then
                        Draw.quad // body of ln
                            ( Quad.ofRect (
                                Rect.Box(left + columnPositions.[k], headpos + noteHeight * 0.5f, column_width, pos - headpos)
                                |> scrollDirectionPos bottom
                              ) )
                            (Quad.colorOf tint)
                            (Sprite.gridUV (animation.Loops, hold_colors.[k]) (Content.getTexture "holdbody"))
                    if headpos - pos < noteHeight * 0.5f then
                        Draw.quad // tail of ln
                            (Quad.ofRect (
                                Rect.Create(left + columnPositions.[k], Math.Max(pos, headpos), left + columnPositions.[k] + column_width, pos + noteHeight)
                                |> scrollDirectionPos bottom
                              ) ) // todo: clipping maths
                            (Quad.colorOf tint)
                            (Sprite.gridUV (animation.Loops, int color.[k]) tailsprite |> fun struct (s, q) -> struct (s, scrollDirectionFlip q))
                    Draw.quad // head of ln
                        (
                            Quad.ofRect ( 
                                Rect.Box(left + columnPositions.[k], headpos, column_width, noteHeight)
                                |> scrollDirectionPos bottom
                            )
                            |> rotation k
                        )
                        (Quad.colorOf tint)
                        (Sprite.gridUV (animation.Loops, hold_colors.[k]) (Content.getTexture "holdhead"))
                    hold_presence.[k] <- false
            note_peek <- note_peek + 1
        
        for k in 0 .. (keys - 1) do
            if hold_presence.[k] then
                let headpos = hold_pos.[k]
                let tint = if hold_pos.[k] = hitposition && state.Scoring.IsHoldDropped hold_index.[k] k then Content.noteskinConfig().DroppedHoldColor else Color.White
                Draw.quad // body of ln, tail is offscreen
                    (
                        Quad.ofRect ( 
                            Rect.Create(
                                left + columnPositions.[k],
                                headpos + noteHeight * 0.5f,
                                left + columnPositions.[k] + column_width,
                                bottom + 2.0f)
                            |> scrollDirectionPos bottom
                        )
                    )
                    (Quad.colorOf tint)
                    (Sprite.gridUV (animation.Loops, hold_colors.[k]) (Content.getTexture "holdbody"))
                Draw.quad // head of ln, tail is offscreen
                    (
                        Quad.ofRect ( 
                            Rect.Box(left + columnPositions.[k], headpos, column_width, noteHeight)
                            |> scrollDirectionPos bottom
                        )
                        |> rotation k
                    )
                    (Quad.colorOf tint)
                    (Sprite.gridUV (animation.Loops, hold_colors.[k]) (Content.getTexture "holdhead"))

        base.Draw()