namespace Interlude.UI

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Prelude
open Prelude.Mods
open Prelude.Calculator.Patterns

type Timeline(with_mods: ModdedChart, on_seek: Time -> unit, rate: Setting.Bounded<Rate>) =
    inherit StaticWidget(NodeType.None)

    let HEIGHT = 60.0f
    let LAST_NOTE = with_mods.LastNote
    let FIRST_NOTE = with_mods.FirstNote
    let bpms = with_mods.BPM

    let BOX_HEIGHT = 100.0f
    let BOX_WIDTH = 200.0f

    let samples =
        int ((LAST_NOTE - FIRST_NOTE) / 1000.0f)
        |> max 10
        |> min 400

    let duration_on_1x = (LAST_NOTE - FIRST_NOTE) |> format_duration_ms

    // chord density is notes per second but n simultaneous notes count for 1 instead of n, aka 'chords per second'
    let note_density, chord_density = Density.nps_cps samples with_mods

    let max_note_density = Array.max note_density

    let mutable dragging = false
    let mutable unpause_after_drag = false
    let mutable bpm_index = 0

    override this.Draw() =
        let b = this.Bounds.Shrink(10.0f, 20.0f)
        let LEADIN_TIME = Song.LEADIN_TIME * rate.Value
        let start = with_mods.FirstNote - LEADIN_TIME
        let offset = b.Width * LEADIN_TIME / LAST_NOTE

        let w = (b.Width - offset) / float32 note_density.Length

        let mutable x = b.Left + offset - w
        let mutable note_prev = 0.0f
        let mutable chord_prev = 0.0f

        for i = 0 to note_density.Length - 1 do
            let note_next = HEIGHT * note_density.[i] / max_note_density
            let chord_next = HEIGHT * chord_density.[i] / max_note_density

            Render.quad_points
                (x, b.Bottom)
                (x, b.Bottom - note_prev)
                (x + w, b.Bottom - note_next)
                (x + w, b.Bottom)
                Colors.white.O2

            Render.quad_points
                (x, b.Bottom)
                (x, b.Bottom - chord_prev)
                (x + w, b.Bottom - chord_next)
                (x + w, b.Bottom)
                Colors.cyan_accent.O1

            x <- x + w
            note_prev <- note_next
            chord_prev <- chord_next

        Render.quad_points
            (x, b.Bottom)
            (x, b.Bottom - note_prev)
            (b.Right, b.Bottom - note_prev)
            (b.Right, b.Bottom)
            Colors.white.O2

        Render.quad_points
            (x, b.Bottom)
            (x, b.Bottom - chord_prev)
            (b.Right, b.Bottom - chord_prev)
            (b.Right, b.Bottom)
            Colors.cyan_accent.O1

        let now = Song.time()
        let percent = (now - start) / (LAST_NOTE - start) |> min 1.0f
        let x = b.Width * percent
        Render.rect (b.BorderB(10.0f)) Colors.grey_1.O3
        Render.rect (b.BorderB(10.0f).SliceL x) Colors.cyan_accent

        let duration = (LAST_NOTE - FIRST_NOTE) / rate.Value |> format_duration_ms
        let now_on_1x = max 0.0f<ms> (now - FIRST_NOTE) |> format_duration_ms
        let now = max 0.0f<ms> (now - FIRST_NOTE) / rate.Value |> format_duration_ms

        Text.draw_aligned_b(Style.font, (sprintf "%.0f%%" (percent * 100.0f)), 24.0f, b.CenterX, b.Bottom - 40.0f, Colors.text, Alignment.CENTER)

        Text.draw_b(Style.font, now, 24.0f, b.Left + 10.0f, b.Bottom - 40.0f, Colors.text)
        Text.draw_aligned_b(Style.font, duration, 24.0f, b.Right - 10.0f, b.Bottom - 40.0f, Colors.text, Alignment.RIGHT)

        if rate.Value <> 1.0f<rate> then
            Text.draw_b(Style.font, now_on_1x, 14.0f, b.Left + 10.0f, b.Bottom - 60.0f, Colors.text_greyout)
            Text.draw_aligned_b(Style.font, duration_on_1x, 14.0f, b.Right - 10.0f, b.Bottom - 60.0f, Colors.text_greyout, Alignment.RIGHT)

        if this.Bounds.Bottom - Mouse.y () < 100.0f then

            let percent = (Mouse.x () - 10.0f) / (Render.width() - 20.0f) |> min 1.0f |> max 0.0f

            let start = FIRST_NOTE - Song.LEADIN_TIME * rate.Value
            let hovered_time = start + (LAST_NOTE - start) * percent |> max 0.0f<ms>

            while bpm_index > 0 && bpms.[bpm_index].Time > hovered_time do
                bpm_index <- bpm_index - 1
            while bpm_index + 1 < bpms.Length && bpms.[bpm_index + 1].Time <= hovered_time do
                bpm_index <- bpm_index + 1

            let box =
                Rect.FromSize(
                    this.Bounds.Left + percent * (this.Bounds.Width - BOX_WIDTH),
                    this.Bounds.Bottom - BOX_HEIGHT - 100.0f,
                    BOX_WIDTH,
                    BOX_HEIGHT
                )

            this.DrawHoverInfo(box, hovered_time)

    member this.DrawHoverInfo(bounds: Rect, time: Time) =
        let bpm = if bpms.Length > 0 then 60000.0f<ms / minute> / bpms.[bpm_index].Data.MsPerBeat * rate.Value else 120f<beat / minute> * rate.Value
        let s_index = (time - FIRST_NOTE) / (LAST_NOTE - FIRST_NOTE) * float32 samples |> floor |> int |> max 0 |> min (samples - 1)
        let nps = note_density.[s_index] * rate.Value
        let cps = chord_density.[s_index] * rate.Value

        let outline_bounds = bounds.Expand(Style.PADDING)
        Render.rect outline_bounds Colors.white.O4
        Render.rect bounds Colors.shadow_2.O4

        let row_height = bounds.Height / 3.0f
        let text_b = bounds.SliceT(row_height).Shrink(20.0f, 0.0f)

        Text.fill_b (
            Style.font,
            (
                if rate.Value <> 1.0f<rate> then
                    sprintf "%s (%s)" (format_duration_ms (time / float32 rate.Value)) (format_duration_ms time)
                else
                    format_duration_ms time
            ),
            text_b,
            Colors.text,
            Alignment.LEFT
        )

        Text.fill_b (
            Style.font,
            sprintf "NPS: %.0f  CPS: %.0f" nps cps,
            text_b.TranslateY(row_height),
            Colors.text_subheading,
            Alignment.LEFT
        )

        Text.fill_b (
            Style.font,
            sprintf "BPM: %.1f" bpm,
            text_b.TranslateY(row_height * 2.0f),
            Colors.text_subheading,
            Alignment.LEFT
        )

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if this.Bounds.Bottom - Mouse.y () < 100.0f && Mouse.left_clicked() then
            dragging <- true
            unpause_after_drag <- Song.playing()
            if unpause_after_drag then
                Song.pause()

        if dragging then
            let percent =
                (Mouse.x () - 10.0f) / (Render.width() - 20.0f) |> min 1.0f |> max 0.0f

            let start = FIRST_NOTE - Song.LEADIN_TIME * rate.Value
            let new_time = start + (LAST_NOTE - start) * percent
            on_seek new_time

            if not (Mouse.held Mouse.LEFT) then
                dragging <- false
                if unpause_after_drag then
                    Song.resume()