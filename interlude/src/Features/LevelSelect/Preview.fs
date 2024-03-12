namespace Interlude.Features.LevelSelect

open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Audio
open Prelude.Common
open Prelude.Charts.Processing.Patterns
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Content
open Interlude.Features.Gameplay.Chart
open Interlude.Features.Play

type Preview(info: LoadedChartInfo, change_rate: float32 -> unit) =
    inherit Dialog()

    let HEIGHT = 60.0f

    // chord density is notes per second but n simultaneous notes count for 1 instead of n
    let samples =
        int ((info.WithMods.LastNote - info.WithMods.FirstNote) / 1000.0f) |> max 10 |> min 400

    let note_density, chord_density = Analysis.nps_cps samples info.WithMods

    let note_density, chord_density =
        Array.map float32 note_density, Array.map float32 chord_density

    let max_note_density = Array.max note_density

    let playfield =
        Playfield(info.WithColors, PlayState.Dummy info, Content.NoteskinConfig, false)
        |+ LaneCover()

    let volume = Volume()
    let mutable dragging = false

    override this.Init(parent: Widget) =
        base.Init parent
        playfield.Init this
        volume.Init this

    override this.Draw() =
        playfield.Draw()

        let b = this.Bounds.Shrink(10.0f, 20.0f)
        let start = info.WithMods.FirstNote - Song.LEADIN_TIME
        let offset = b.Width * Song.LEADIN_TIME / info.WithMods.LastNote

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

        let percent = (Song.time () - start) / (info.WithMods.LastNote - start) |> min 1.0f
        let x = b.Width * percent
        Draw.rect (b.SliceBottom(5.0f)) (Color.FromArgb(160, Color.White))
        Draw.rect (b.SliceBottom(5.0f).SliceLeft x) (Palette.color (255, 1.0f, 0.0f))

        volume.Draw()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        volume.Update(elapsed_ms, moved)
        playfield.Update(elapsed_ms, moved)

        if this.Bounds.Bottom - Mouse.y () < 200.0f && Mouse.left_click () then
            dragging <- true
            Song.pause ()

        if dragging then
            let percent =
                (Mouse.x () - 10.0f) / (Viewport.vwidth - 20.0f) |> min 1.0f |> max 0.0f

            let start = info.WithMods.FirstNote - Song.LEADIN_TIME
            let new_time = start + (info.WithMods.LastNote - start) * percent
            Song.seek new_time

        if not (Mouse.held Mouse.LEFT) then
            dragging <- false
            Song.resume ()

        if (%%"preview").Tapped() || (%%"exit").Tapped() || Mouse.released Mouse.RIGHT then
            this.Close()
        elif (%%"select").Tapped() then
            this.Close()
            LevelSelect.choose_this_chart ()
        elif (%%"uprate_small").Tapped() then
            change_rate (0.01f)
        elif (%%"uprate_half").Tapped() then
            change_rate (0.05f)
        elif (%%"uprate").Tapped() then
            change_rate (0.1f)
        elif (%%"downrate_small").Tapped() then
            change_rate (-0.01f)
        elif (%%"downrate_half").Tapped() then
            change_rate (-0.05f)
        elif (%%"downrate").Tapped() then
            change_rate (-0.1f)
        elif (%%"screenshot").Tapped() then Toolbar.take_screenshot()

    override this.Close() =
        if dragging then
            Song.resume ()

        base.Close()
