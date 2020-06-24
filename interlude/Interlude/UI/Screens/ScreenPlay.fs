namespace Interlude.UI

open OpenTK
open Prelude.Charts.Interlude
open Interlude
open Interlude.Render
open Interlude.Options

type NoteRenderer() as this =
    inherit Widget()
    //scale, column width, note provider should be options
    
    //functions to get bounding boxes for things. used to place other gameplay widgets on the playfield.

    let (keys, notes, bpm, sv, mods) = Gameplay.modifiedChart.Force()
    let columnPositions = Array.init keys (fun i -> float32 i * Themes.noteskinConfig.ColumnWidth)
    let columnWidths = Array.create keys (float32 Themes.noteskinConfig.ColumnWidth)
    let noteHeight = Themes.noteskinConfig.ColumnWidth
    let scale = Options.profile.ScrollSpeed.Get() / Gameplay.rate
    let hitposition = float32 <| Options.profile.HitPosition.Get()

    do
        //todo: position differently for editor
        let width = Array.mapi (fun i n -> n + columnWidths.[i]) columnPositions |> Array.max
        let (screenAlign, columnAlign) = Themes.noteskinConfig.PlayfieldAlignment
        this.Reposition(-width * columnAlign, screenAlign, 0.0f, 0.0f, width * (1.0f - columnAlign), screenAlign, 0.0f, 1.0f)

    override this.Draw() =
        let struct (left, top, right, bottom) = this.Bounds
        let playfieldHeight = bottom - top
        let now = Audio.timeWithOffset()

        for k in 0 .. (keys - 1) do
            Draw.rect(Rect.create (left + columnPositions.[k]) top (left + columnPositions.[k] + columnWidths.[k]) bottom) (Color.FromArgb(127, 0, 0, 0)) Sprite.Default

        for (time, nd) in notes.EnumerateBetween(now - 200.0) (now + 1500.0) do //ranges
            let pos = bottom - hitposition - float32 ((time - now) * scale)
            for k in 0 .. (keys - 1) do
                if testForNote k NoteType.NORMAL nd then
                    Draw.rect (Rect.create(left + columnPositions.[k]) (pos - noteHeight) (left + columnPositions.[k] + columnWidths.[k]) pos) Color.White <| Themes.getTexture("note")
            


type ScreenPlay() as this =
    inherit Screen()
    
    do
        this.Add(new NoteRenderer())

    override this.OnEnter(prev) =
        if (prev :? ScreenScore) then
            Screens.popScreen()
        else
            Screens.backgroundDim.SetTarget(Options.profile.BackgroundDim.Get() |> float32)
            //discord prescence
            Screens.setToolbarCollapsed(true)
            //disable cursor
            //banner animation
            Audio.changeRate(Gameplay.rate)
            Audio.playLeadIn()

    override this.OnExit(next) =
        Screens.setToolbarCollapsed(false)
        Screens.backgroundDim.SetTarget(0.7f)