namespace Interlude.UI

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Prelude
open Prelude.Charts.Processing
open Prelude.Charts.Processing.Patterns

type Timeline(with_mods: ModdedChart, on_seek: Time -> unit, rate: Setting.Bounded<float32>) =
    inherit StaticWidget(NodeType.None)

    let HEIGHT = 60.0f
    let LAST_NOTE = with_mods.LastNote
    let FIRST_NOTE = with_mods.FirstNote

    let samples =
        int ((LAST_NOTE - FIRST_NOTE) / 1000.0f)
        |> max 10
        |> min 400

    let duration_on_1x = (LAST_NOTE - FIRST_NOTE) |> format_duration_ms

    // chord density is notes per second but n simultaneous notes count for 1 instead of n, aka 'chords per second'
    let note_density, chord_density = Analysis.nps_cps samples with_mods

    let note_density, chord_density =
        Array.map float32 note_density, Array.map float32 chord_density

    let max_note_density = Array.max note_density

    let mutable dragging = false
    let mutable unpause_after_drag = false

    override this.Draw() =
        let b = this.Bounds.Shrink(10.0f, 20.0f)
        let start = with_mods.FirstNote - Song.LEADIN_TIME
        let offset = b.Width * Song.LEADIN_TIME / LAST_NOTE

        let w = (b.Width - offset) / float32 note_density.Length

        let mutable x = b.Left + offset - w
        let mutable note_prev = 0.0f
        let mutable chord_prev = 0.0f

        for i = 0 to note_density.Length - 1 do
            let note_next = HEIGHT * note_density.[i] / max_note_density
            let chord_next = HEIGHT * chord_density.[i] / max_note_density

            Draw.untextured_quad
                (Quad.createv (x, b.Bottom) (x, b.Bottom - note_prev) (x + w, b.Bottom - note_next) (x + w, b.Bottom))
                Colors.white.O2.AsQuad

            Draw.untextured_quad
                (Quad.createv (x, b.Bottom) (x, b.Bottom - chord_prev) (x + w, b.Bottom - chord_next) (x + w, b.Bottom))
                Colors.cyan_accent.O1.AsQuad

            x <- x + w
            note_prev <- note_next
            chord_prev <- chord_next

        Draw.untextured_quad
            (Quad.createv (x, b.Bottom) (x, b.Bottom - note_prev) (b.Right, b.Bottom - note_prev) (b.Right, b.Bottom))
            Colors.white.O2.AsQuad

        Draw.untextured_quad
            (Quad.createv (x, b.Bottom) (x, b.Bottom - chord_prev) (b.Right, b.Bottom - chord_prev) (b.Right, b.Bottom))
            Colors.cyan_accent.O1.AsQuad

        let now = Song.time()
        let percent = (now - start) / (LAST_NOTE - start) |> min 1.0f
        let x = b.Width * percent
        Draw.rect (b.BorderBottom(10.0f)) Colors.grey_1.O3
        Draw.rect (b.BorderBottom(10.0f).SliceLeft x) Colors.cyan_accent

        let duration = (LAST_NOTE - FIRST_NOTE) / rate.Value |> format_duration_ms
        let now_on_1x = max 0.0f<ms> (now - FIRST_NOTE) |> format_duration_ms
        let now = max 0.0f<ms> (now - FIRST_NOTE) / rate.Value |> format_duration_ms

        Text.draw_aligned_b(Style.font, (sprintf "%.0f%%" (percent * 100.0f)), 24.0f, b.CenterX, b.Bottom - 40.0f, Colors.text, Alignment.CENTER)

        Text.draw_b(Style.font, now, 24.0f, b.Left + 10.0f, b.Bottom - 40.0f, Colors.text)
        Text.draw_aligned_b(Style.font, duration, 24.0f, b.Right - 10.0f, b.Bottom - 40.0f, Colors.text, Alignment.RIGHT)

        if rate.Value <> 1.0f then
            Text.draw_b(Style.font, now_on_1x, 14.0f, b.Left + 10.0f, b.Bottom - 60.0f, Colors.text_greyout)
            Text.draw_aligned_b(Style.font, duration_on_1x, 14.0f, b.Right - 10.0f, b.Bottom - 60.0f, Colors.text_greyout, Alignment.RIGHT)

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if this.Bounds.Bottom - Mouse.y () < 200.0f && Mouse.left_click () then
            dragging <- true
            unpause_after_drag <- Song.playing()
            if unpause_after_drag then
                Song.pause()

        if dragging then
            let percent =
                (Mouse.x () - 10.0f) / (Viewport.vwidth - 20.0f) |> min 1.0f |> max 0.0f

            let start = FIRST_NOTE - Song.LEADIN_TIME
            let new_time = start + (LAST_NOTE - start) * percent
            on_seek new_time

        if not (Mouse.held Mouse.LEFT) then
            dragging <- false
            if unpause_after_drag then
                Song.resume()