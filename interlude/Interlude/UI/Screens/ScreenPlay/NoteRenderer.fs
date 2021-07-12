namespace Interlude.UI

open OpenTK
open System
open System.Drawing
open Prelude.Common
open Prelude.Charts.Interlude
open Prelude.Scoring
open Prelude.Data.Themes
open Interlude
open Interlude.Graphics
open Interlude.Options
open Interlude.UI.Animation

// TODO LIST
//  COLUMN INDEPENDENT SV
//  MAYBE FIX HOLD TAIL CLIPPING
//  NOTE PROVIDER SYSTEM

(*
    Note rendering code. Designed so it could be repurposed as an editor but that may now never happen (editor will be another project)
*)

type NoteRenderer(scoring: IScoreMetric) as this =
    inherit Widget()

    //constants
    let (keys, notes, bpm, sv, mods) = Gameplay.getColoredChart()
    let columnPositions = Array.init keys (fun i -> float32 i * Themes.noteskinConfig.ColumnWidth)
    let columnWidths = Array.create keys (float32 Themes.noteskinConfig.ColumnWidth)
    let noteHeight = Themes.noteskinConfig.ColumnWidth
    let holdnoteTrim = Themes.noteskinConfig.ColumnWidth * Themes.noteskinConfig.HoldNoteTrim
    let playfieldColor = Themes.noteskinConfig.PlayfieldColor

    let tailsprite = Themes.getTexture(if Themes.noteskinConfig.UseHoldTailTexture then "holdtail" else "holdhead")
    let animation = new AnimationCounter(200.0)

    // arrays of stuff that are reused/changed every frame. the data from the previous frame is not used, but making new arrays causes garbage collection
    let mutable note_seek = 0 // see comments for sv_seek and sv_peek. same role but for index of next row
    let mutable note_peek = note_seek
    let sv = Array.init (keys + 1) (fun i -> sv.GetChannelData(i - 1).Data)
    let sv_seek = Array.create (keys + 1) 0 // index of next appearing SV point for each channel; = number of SV points if there are no more
    let sv_peek = Array.create (keys + 1) 0 // same as sv_seek, but sv_seek stores the relevant point for t = now (according to music) and this stores t > now
    let sv_value = Array.create (keys + 1) 1.0f // value of the most recent sv point per channel, equal to the sv value at index (sv_peek - 1), or 1 if that point doesn't exist
    let sv_time = Array.zeroCreate (keys + 1)
    let column_pos = Array.zeroCreate keys // running position calculation of notes for sv
    let hold_presence = Array.create keys false
    let hold_pos = Array.create keys 0.0f
    let hold_colors = Array.create keys 0

    let scrollDirectionPos bottom = if Options.options.Upscroll.Value then id else fun (struct (l, t, r, b): Rect) -> struct (l, bottom - b, r, bottom - t)
    let scrollDirectionFlip = fun q -> if (not Themes.noteskinConfig.FlipHoldTail) || Options.options.Upscroll.Value then q else Quad.flip q
    let noteRotation =
        if keys = 4 && Themes.noteskinConfig.UseRotation then
            fun k -> Quad.rotateDeg (match k with 0 -> 90.0 | 1 -> 0.0 | 2 -> 180.0 | 3 -> 270.0 | _ -> 0.0)
        elif keys = 6 && Themes.noteskinConfig.UseRotation then
            fun k -> Quad.rotateDeg (match k with 0 -> 90.0 | 1 -> 135.0 | 2 -> 0.0 | 3 -> 180.0 | 4 -> 225.0 | 5 -> 270.0 | _ -> 0.0)
        else fun k -> id

    do
        let width = Array.mapi (fun i n -> n + columnWidths.[i]) columnPositions |> Array.max
        let (screenAlign, columnAlign) = Themes.noteskinConfig.PlayfieldAlignment
        this.Reposition(-width * columnAlign, screenAlign, 0.0f, 0.0f, width * (1.0f - columnAlign), screenAlign, 0.0f, 1.0f)
        this.Animation.Add(animation)

    override this.Draw() =
        let struct (left, top, right, bottom) = this.Bounds
        
        let scale = float32 Options.options.ScrollSpeed.Value / Gameplay.rate * 1.0f</ms>
        let hitposition = float32 Options.options.HitPosition.Value

        let playfieldHeight = bottom - top
        let now = Audio.timeWithOffset()

        // seek to appropriate sv and note locations in data.
        // all of this stuff could be wrapped in an object handling seeking/peeking but would give slower performance because it's based on Seq and not ResizeArray
        // i therefore sadly had to make a mess here. see comments on the variables for more on whats going on
        while note_seek < notes.Data.Count && (offsetOf notes.Data.[note_seek]) < now do
            note_seek <- note_seek + 1
        note_peek <- note_seek
        for i in 0 .. keys do
            while sv_seek.[i] < sv.[i].Count && (offsetOf <| sv.[i].[sv_seek.[i]]) < now do
                sv_seek.[i] <- sv_seek.[i] + 1
            sv_peek.[i] <- sv_seek.[i]
            sv_value.[i] <- if sv_seek.[i] > 0 then snd sv.[i].[sv_seek.[i] - 1] else 1.0f

        for k in 0 .. (keys - 1) do
            Draw.rect(Rect.create (left + columnPositions.[k]) top (left + columnPositions.[k] + columnWidths.[k]) bottom) playfieldColor Sprite.Default
            sv_time.[k] <- now
            column_pos.[k] <- hitposition
            hold_pos.[k] <- hitposition
            hold_presence.[k] <-
                if note_seek > 0 then
                    let (_, struct (nd, c)) = notes.Data.[note_seek - 1] in
                    hold_colors.[k] <- int c.[k]
                    (NoteRow.hasNote k NoteType.HOLDHEAD nd || NoteRow.hasNote k NoteType.HOLDBODY nd)
                else false
            Draw.quad // receptor
                (Rect.create (left + columnPositions.[k]) hitposition (left + columnPositions.[k] + columnWidths.[k]) (hitposition + noteHeight) |> scrollDirectionPos bottom |> Quad.ofRect |> noteRotation k)
                (Color.White |> Quad.colorOf)
                (Sprite.gridUV (animation.Loops, if (scoring.KeyState |> Bitmap.hasBit k) then 1 else 0) (Themes.getTexture "receptor"))

        // main render loop - until the last note rendered in every column appears off screen
        let mutable min = hitposition
        while min < playfieldHeight && note_peek < notes.Data.Count do
            min <- playfieldHeight
            let (t, struct (nd, color)) = notes.Data.[note_peek]
            // until no sv adjustments needed...
            // update main sv
            while (sv_peek.[0] < sv.[0].Count && offsetOf sv.[0].[sv_peek.[0]] < t) do
                let (t2, v) = sv.[0].[sv_peek.[0]]
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
                if NoteRow.hasNote k NoteType.NORMAL nd then
                    Draw.quad // normal note
                        (Quad.ofRect (Rect.create(left + columnPositions.[k]) column_pos.[k] (left + columnPositions.[k] + columnWidths.[k]) (column_pos.[k] + noteHeight) |> scrollDirectionPos bottom) |> noteRotation k)
                        (Quad.colorOf Color.White)
                        (Sprite.gridUV (animation.Loops, int color.[k]) (Themes.getTexture "note"))
                elif NoteRow.hasNote k NoteType.HOLDHEAD nd then
                    hold_pos.[k] <- column_pos.[k]
                    hold_colors.[k] <- int color.[k]
                    hold_presence.[k] <- true
                elif NoteRow.hasNote k NoteType.HOLDTAIL nd then
                    let headpos = hold_pos.[k]
                    let pos = column_pos.[k] - holdnoteTrim
                    if headpos < pos then
                        Draw.quad // body of ln
                            (Quad.ofRect (Rect.create(left + columnPositions.[k]) (headpos + noteHeight * 0.5f) (left + columnPositions.[k] + columnWidths.[k]) (pos + noteHeight * 0.5f) |> scrollDirectionPos bottom))
                            (Quad.colorOf Color.White)
                            (Sprite.gridUV (animation.Loops, hold_colors.[k]) (Themes.getTexture "holdbody"))
                    if headpos - pos < noteHeight * 0.5f then
                        Draw.quad // tail of ln
                            (Quad.ofRect (Rect.create(left + columnPositions.[k]) (Math.Max(pos, headpos)) (left + columnPositions.[k] + columnWidths.[k]) (pos + noteHeight) |> scrollDirectionPos bottom)) // todo: clipping maths
                            (Quad.colorOf Color.White)
                            (Sprite.gridUV (animation.Loops, int color.[k]) tailsprite |> fun struct (s, q) -> struct (s, scrollDirectionFlip q))
                    Draw.quad // head of ln
                        (Quad.ofRect (Rect.create(left + columnPositions.[k]) headpos (left + columnPositions.[k] + columnWidths.[k]) (headpos + noteHeight) |> scrollDirectionPos bottom) |> noteRotation k)
                        (Quad.colorOf Color.White)
                        (Sprite.gridUV (animation.Loops, hold_colors.[k]) (Themes.getTexture "holdhead"))
                    hold_presence.[k] <- false
                elif NoteRow.hasNote k NoteType.MINE nd then
                    Draw.quad // mine
                        (Quad.ofRect (Rect.create(left + columnPositions.[k]) column_pos.[k] (left + columnPositions.[k] + columnWidths.[k]) (column_pos.[k] + noteHeight) |> scrollDirectionPos bottom))
                        (Quad.colorOf Color.White)
                        (Sprite.gridUV (animation.Loops, int color.[k]) (Themes.getTexture "mine"))
            note_peek <- note_peek + 1
        
        for k in 0 .. (keys - 1) do
            if hold_presence.[k] then
                let headpos = hold_pos.[k]
                Draw.quad // body of ln, tail is offscreen
                    (Quad.ofRect (Rect.create(left + columnPositions.[k]) (headpos + noteHeight * 0.5f) (left + columnPositions.[k] + columnWidths.[k]) bottom |> scrollDirectionPos bottom))
                    (Quad.colorOf Color.White)
                    (Sprite.gridUV (animation.Loops, hold_colors.[k]) (Themes.getTexture "holdbody"))
                Draw.quad // head of ln, tail is offscreen
                    (Quad.ofRect (Rect.create(left + columnPositions.[k]) headpos (left + columnPositions.[k] + columnWidths.[k]) (headpos + noteHeight) |> scrollDirectionPos bottom) |> noteRotation k)
                    (Quad.colorOf Color.White)
                    (Sprite.gridUV (animation.Loops, hold_colors.[k]) (Themes.getTexture "holdhead"))
        base.Draw()
