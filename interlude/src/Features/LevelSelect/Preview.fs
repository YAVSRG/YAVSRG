namespace Interlude.Features.LevelSelect

open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Audio
open Prelude.Common
open Prelude.Charts.Tools
open Prelude.Charts.Tools.Patterns
open Interlude.UI.Menu
open Interlude.Features.Play

type Preview(chart: ModChart, changeRate: float32 -> unit) =
    inherit Dialog()

    let density_graph_1, density_graph_2 = Analysis.nps_cps 100 chart

    let density_graph_1, density_graph_2 =
        Array.map float32 density_graph_1, Array.map float32 density_graph_2

    let max_note_density = Array.max density_graph_1

    let playfield = Playfield(PlayState.Dummy chart, false) |+ LaneCover()

    let volume = Volume()
    let mutable dragging = false

    override this.Init(parent: Widget) =
        base.Init parent
        playfield.Init this
        volume.Init this

    override this.Draw() =
        playfield.Draw()

        let b = this.Bounds.Shrink(10.0f, 20.0f)
        let start = chart.FirstNote - Song.LEADIN_TIME

        let w = b.Width / float32 density_graph_1.Length

        for i = 0 to density_graph_1.Length - 1 do
            let h = 80.0f * density_graph_1.[i] / max_note_density
            let h2 = 80.0f * density_graph_2.[i] / max_note_density
            Draw.rect (Rect.Box(b.Left + float32 i * w, b.Bottom - h, w, h - 5.0f)) (Color.FromArgb(120, Color.White))
            Draw.rect (Rect.Box(b.Left + float32 i * w, b.Bottom - h2, w, h2 - 5.0f)) (Palette.color (80, 1.0f, 0.0f))

        let percent = (Song.time () - start) / (chart.LastNote - start)
        Draw.rect (b.SliceBottom(5.0f)) (Color.FromArgb(160, Color.White))
        let x = b.Width * percent
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

            let start = chart.FirstNote - Song.LEADIN_TIME
            let newTime = start + (chart.LastNote - start) * percent
            Song.seek newTime

        if not (Mouse.held Mouse.LEFT) then
            dragging <- false
            Song.resume ()

        if (+."preview").Tapped() || (+."exit").Tapped() || Mouse.released Mouse.RIGHT then
            this.Close()
        elif (+."select").Tapped() then
            this.Close()
            LevelSelect.play ()
        elif (+."uprate_small").Tapped() then
            changeRate (0.01f)
        elif (+."uprate_half").Tapped() then
            changeRate (0.05f)
        elif (+."uprate").Tapped() then
            changeRate (0.1f)
        elif (+."downrate_small").Tapped() then
            changeRate (-0.01f)
        elif (+."downrate_half").Tapped() then
            changeRate (-0.05f)
        elif (+."downrate").Tapped() then
            changeRate (-0.1f)

    override this.Close() =
        if dragging then
            Song.resume ()

        base.Close()
