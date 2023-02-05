namespace Interlude.Features.LevelSelect

open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Audio
open Prelude.Common
open Prelude.Scoring
open Prelude.Charts.Formats.Interlude
open Interlude.Features.Play

type Preview(chart: Chart) =
    inherit Dialog()

    let density_graph_1, density_graph_2 = Prelude.Charts.Tools.Patterns.Analysis.density 100 chart
    let density_graph_1, density_graph_2 = Array.map float32 density_graph_1, Array.map float32 density_graph_2
    let max_note_density = Array.max density_graph_1

    let renderer =
        NoteRenderer(Metrics.createDummyMetric chart)
        |+ GameplayWidgets.LaneCover()

    override this.Init(parent: Widget) =
        base.Init parent
        renderer.Init this

    override this.Draw() =
        renderer.Draw()

        let w = this.Bounds.Width / float32 density_graph_1.Length
        for i = 0 to density_graph_1.Length - 1 do
            let h = 200.0f * density_graph_1.[i] / max_note_density
            let h2  = 200.0f * density_graph_2.[i] / max_note_density
            Draw.rect (Rect.Box(this.Bounds.Left + float32 i * w, this.Bounds.Bottom - h, w, h - 10.0f)) (Color.FromArgb(120, Color.White))
            Draw.rect (Rect.Box(this.Bounds.Left + float32 i * w, this.Bounds.Bottom - h2, w, h2 - 10.0f)) (Style.color(80, 1.0f, 0.0f))

        let percent = (Song.time() - chart.FirstNote) / (chart.LastNote - chart.FirstNote) 
        Draw.rect (this.Bounds.SliceBottom(10.0f)) (Style.color(255, 0.4f, 0.0f))
        let x = this.Bounds.Width * percent
        Draw.rect (this.Bounds.SliceBottom(10.0f).SliceLeft x) (Style.color(255, 1.0f, 0.0f))
        Draw.rect (Rect.Create(this.Bounds.Left + x - 2.5f, this.Bounds.Bottom - 200.0f, this.Bounds.Left + x + 2.5f, this.Bounds.Bottom)) Color.White

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        renderer.Update(elapsedTime, moved)
        if Mouse.leftClick() then
            let percent = Mouse.x() / Viewport.vwidth
            let newTime = chart.FirstNote + (chart.LastNote - chart.FirstNote) * percent
            Song.seek newTime
        if (!|"preview").Tapped() || (!|"exit").Tapped() then
            this.Close()