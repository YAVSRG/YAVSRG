namespace Interlude.UI

open OpenTK
open System
open Prelude.Common
open Prelude.Charts.Interlude
open Interlude
open Interlude.Render
open Interlude.Options

//TODO LIST
//  UPSCROLL SUPPORT
//  ANIMATION SUPPORT
//  SEEKING/REWINDING SUPPORT
//  RECEPTORS
//  COLUMN INDEPENDENT SV
//  MAYBE FIX HOLD TAIL CLIPPING


type NoteRenderer() as this =
    inherit Widget()
    //scale, column width, note provider should be options
    
    //functions to get bounding boxes for things. used to place other gameplay widgets on the playfield.

    //constants
    let (keys, notes, bpm, sv, mods) = Gameplay.coloredChart.Force()
    let columnPositions = Array.init keys (fun i -> float32 i * Themes.noteskinConfig.ColumnWidth)
    let columnWidths = Array.create keys (float32 Themes.noteskinConfig.ColumnWidth)
    let noteHeight = Themes.noteskinConfig.ColumnWidth
    let scale = float32(Options.profile.ScrollSpeed.Get()) / Gameplay.rate * 1.0f</ms>
    let hitposition = float32 <| Options.profile.HitPosition.Get()
    let holdnoteTrim = Themes.noteskinConfig.ColumnWidth * Themes.noteskinConfig.HoldNoteTrim

    let animation = new Animation.AnimationCounter(200.0)

    //arrays of stuff that are reused/changed every frame. the data from the previous frame is not used, but making new arrays causes garbage collection
    let mutable note_seek = 0 //see comments for sv_seek and sv_peek. same role but for index of next row
    let mutable note_peek = note_seek
    let sv = Array.init (keys + 1) (fun i -> sv.GetChannelData(i - 1).Data)
    let sv_seek = Array.create (keys + 1) 0 //index of next appearing SV point for each channel; = number of SV points if there are no more
    let sv_peek = Array.create (keys + 1) 0 //same as sv_seek, but sv_seek stores the relevant point for t = now (according to music) and this stores t > now
    let sv_value = Array.create (keys + 1) 1.0f //value of the most recent sv point per channel, equal to the sv value at index (sv_peek - 1), or 1 if that point doesn't exist
    let sv_time = Array.zeroCreate (keys + 1)
    let column_pos = Array.zeroCreate keys //running position calculation of notes for sv
    let hold_presence = Array.create keys false
    let hold_pos = Array.create keys 0.0f
    let hold_colors = Array.create keys 0

    do
        //todo: position differently for editor
        let width = Array.mapi (fun i n -> n + columnWidths.[i]) columnPositions |> Array.max
        let (screenAlign, columnAlign) = Themes.noteskinConfig.PlayfieldAlignment
        this.Reposition(-width * columnAlign, screenAlign, 0.0f, 0.0f, width * (1.0f - columnAlign), screenAlign, 0.0f, 1.0f)
        this.Animation.Add(animation)

    override this.Draw() =
        let struct (left, top, right, bottom) = this.Bounds
        let playfieldHeight = bottom - top
        let now = Audio.timeWithOffset()

        //seek to appropriate sv and note locations in data.
        //all of this stuff could be wrapped in an object handling seeking/peeking but would give slower performance because it's based on Seq and not ResizeArray
        //i therefore sadly had to make a mess here. see comments on the variables for more on whats going on
        while note_seek < notes.Data.Count && (offsetOf notes.Data.[note_seek]) < now do
            note_seek <- note_seek + 1
        note_peek <- note_seek
        for i in 0 .. keys do
            while sv_seek.[i] < sv.[i].Count && (offsetOf <| sv.[i].[sv_seek.[i]]) < now do
                sv_seek.[i] <- sv_seek.[i] + 1
            sv_peek.[i] <- sv_seek.[i]
            sv_value.[i] <- if sv_seek.[i] > 0 then snd sv.[i].[sv_seek.[i] - 1] else 1.0f

        for k in 0 .. (keys - 1) do
            Draw.rect(Rect.create (left + columnPositions.[k]) top (left + columnPositions.[k] + columnWidths.[k]) bottom) (Color.FromArgb(127, 0, 0, 0)) Sprite.Default
            sv_time.[k] <- now
            column_pos.[k] <- hitposition
            hold_pos.[k] <- hitposition
            hold_presence.[k] <-
                if note_seek > 0 then
                    let (_, struct (nd, c)) = notes.Data.[note_seek - 1] in
                    hold_colors.[k] <- int c.[k]
                    (testForNote k NoteType.HOLDHEAD nd || testForNote k NoteType.HOLDBODY nd)
                else false
            Draw.rect(Rect.create (left + columnPositions.[k]) (bottom - hitposition - noteHeight) (left + columnPositions.[k] + columnWidths.[k]) (bottom - hitposition)) Color.White (Themes.getTexture("receptor")) //animation for being pressed

        //main render loop - until the last note rendered in every column appears off screen
        let mutable min = hitposition
        while min < playfieldHeight && note_peek < notes.Data.Count do
            min <- playfieldHeight
            let (t, struct (nd, color)) = notes.Data.[note_peek]
            //until no sv adjustments needed...
            //update main sv
            while (sv_peek.[0] < sv.[0].Count && offsetOf sv.[0].[sv_peek.[0]] < t) do
                let (t2, v) = sv.[0].[sv_peek.[0]]
                for k in 0 .. (keys - 1) do
                    column_pos.[k] <- column_pos.[k] + scale * sv_value.[0] * (t2 - sv_time.[k])
                    sv_time.[k] <- t2
                sv_value.[0] <- v
                sv_peek.[0] <- sv_peek.[0] + 1
            //update column sv

            //render notes
            for k in 0 .. (keys - 1) do
                column_pos.[k] <- column_pos.[k] + scale * sv_value.[0] * (t - sv_time.[k])
                sv_time.[k] <- t
                min <- Math.Min(column_pos.[k], min)
                if testForNote k NoteType.NORMAL nd then
                    let pos = bottom - column_pos.[k] //SCROLL
                    Draw.quad (Quad.ofRect (Rect.create(left + columnPositions.[k]) (pos - noteHeight) (left + columnPositions.[k] + columnWidths.[k]) pos)) (Quad.colorOf Color.White) (Sprite.uv(animation.Loops, int color.[k])(Themes.getTexture("note")))
                elif testForNote k NoteType.HOLDHEAD nd then
                    hold_pos.[k] <- column_pos.[k]
                    hold_colors.[k] <- int color.[k]
                    hold_presence.[k] <- true
                elif testForNote k NoteType.HOLDTAIL nd then
                    let headpos = bottom - hold_pos.[k]
                    let pos = bottom - column_pos.[k] + holdnoteTrim //SCROLL
                    if headpos > pos then //SCROLL
                        Draw.quad (Quad.ofRect (Rect.create(left + columnPositions.[k]) (pos - noteHeight * 0.5f) (left + columnPositions.[k] + columnWidths.[k]) (headpos - noteHeight * 0.5f))) (Quad.colorOf Color.White) (Sprite.uv(animation.Loops, hold_colors.[k])(Themes.getTexture("holdbody")))
                    if pos - headpos < noteHeight * 0.5f then
                        Draw.quad
                            (Quad.ofRect (Rect.create(left + columnPositions.[k]) (pos - noteHeight) (left + columnPositions.[k] + columnWidths.[k]) (Math.Min(pos, headpos - noteHeight * 0.5f))))
                            (Quad.colorOf Color.White)
                            (Sprite.uv(animation.Loops, int color.[k])(Themes.getTexture("holdtail")))
                    Draw.quad (Quad.ofRect (Rect.create(left + columnPositions.[k]) (headpos - noteHeight) (left + columnPositions.[k] + columnWidths.[k]) headpos)) (Quad.colorOf Color.White) (Sprite.uv(animation.Loops, hold_colors.[k])(Themes.getTexture("holdhead")))
                    hold_presence.[k] <- false
                elif testForNote k NoteType.MINE nd then
                    let pos = bottom - column_pos.[k] //SCROLL
                    Draw.quad (Quad.ofRect (Rect.create(left + columnPositions.[k]) (pos - noteHeight) (left + columnPositions.[k] + columnWidths.[k]) pos)) (Quad.colorOf Color.White) (Sprite.uv(animation.Loops, int color.[k])(Themes.getTexture("mine")))
                    
            note_peek <- note_peek + 1
        
        for k in 0 .. (keys - 1) do
            if hold_presence.[k] then
                let headpos = bottom - hold_pos.[k]
                Draw.quad (Quad.ofRect (Rect.create(left + columnPositions.[k]) top (left + columnPositions.[k] + columnWidths.[k]) (headpos - noteHeight * 0.5f))) (Quad.colorOf Color.White) (Sprite.uv(animation.Loops, hold_colors.[k])(Themes.getTexture("holdbody")))
                Draw.quad (Quad.ofRect (Rect.create(left + columnPositions.[k]) (headpos - noteHeight) (left + columnPositions.[k] + columnWidths.[k]) headpos)) (Quad.colorOf Color.White) (Sprite.uv(animation.Loops, hold_colors.[k])(Themes.getTexture("holdhead")))
                
            


type ScreenPlay() as this =
    inherit Screen()
    
    do
        this.Add(new NoteRenderer())

    override this.OnEnter(prev) =
        if (prev :? ScreenScore) then
            Screens.popScreen()
        else
            Screens.backgroundDim.SetTarget(Options.profile.BackgroundDim.Get() |> float32)
            //discord presence
            Screens.setToolbarCollapsed(true)
            //disable cursor
            //banner animation
            Audio.changeRate(Gameplay.rate)
            Audio.playLeadIn()

    override this.OnExit(next) =
        Screens.setToolbarCollapsed(false)
        Screens.backgroundDim.SetTarget(0.7f)