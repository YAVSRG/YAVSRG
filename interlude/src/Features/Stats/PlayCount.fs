namespace Interlude.Features.Stats

open System
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Interlude.UI

type PlayCount(plays: unit -> int, completed: unit -> int, retries: unit -> int, quits: unit -> int) =
    inherit StaticWidget(NodeType.None)

    let completed_slider = Animation.Fade(0.0f)
    let retries_slider = Animation.Fade(0.0f)
    let quits_slider = Animation.Fade(0.0f)

    let CONTENT_HEIGHT = 165.0f

    override this.Draw() =
        Render.rect this.Bounds Colors.shadow_2.O2

        let graph_radius = min (this.Bounds.Height * 0.4f) (this.Bounds.Width * 0.15f)
        let graph_thickness = graph_radius * 0.3f
        let content_width = graph_radius * 3.0f
        let padding = 15f + graph_radius * 0.05f

        let overall_width = content_width + graph_radius * 2.0f + padding

        let graph_origin = this.Bounds.Left + 0.5f * (this.Bounds.Width - overall_width) + graph_radius
        let content_bounds = this.Bounds.SliceX(overall_width).SliceY(CONTENT_HEIGHT).SliceR(content_width).TranslateY(-5.0f)

        Wedge.draw (graph_origin, this.Bounds.CenterY) (graph_radius - graph_thickness) graph_radius
            0.0 (float completed_slider.Value)
            Colors.green_accent
        Wedge.draw (graph_origin, this.Bounds.CenterY) (graph_radius - graph_thickness) graph_radius
            (float completed_slider.Value) (float completed_slider.Value + float retries_slider.Value)
            Colors.yellow_accent
        Wedge.draw (graph_origin, this.Bounds.CenterY) (graph_radius - graph_thickness) graph_radius
            (float completed_slider.Value + float retries_slider.Value) (float completed_slider.Value + float retries_slider.Value + float quits_slider.Value)
            Colors.red_accent

        let plays = plays()
        let completed = completed()
        let retries = retries()
        let quits = quits()

        if plays = 0 && quits = 0 && completed = 0 && retries = 0 then
            Text.fill_b(Style.font, Icons.PIE_CHART, Rect.FromSize(graph_origin, this.Bounds.CenterY, 0.0f, 0.0f).Expand(graph_radius), Colors.text_greyout, Alignment.CENTER)

        Text.fill_b(Style.font, sprintf "%s: %i" (%"stats.songs_played") plays, content_bounds.SliceT(45.0f), Colors.text, Alignment.LEFT)

        let row = content_bounds.ShrinkT(50.0f).SliceT(35.0f)
        Render.rect (row.SliceL(35.0f).Shrink(10.0f)) Colors.green_accent
        Text.fill_b(Style.font, sprintf "%s: %i" (%"stats.songs_completed") completed, row.ShrinkL(35.0f), Colors.text_subheading, Alignment.LEFT)

        let row = content_bounds.ShrinkT(90.0f).SliceT(35.0f)
        Render.rect (row.SliceL(35.0f).Shrink(10.0f)) Colors.yellow_accent
        Text.fill_b(Style.font, sprintf "%s: %i" (%"stats.songs_retried") retries, row.ShrinkL(35.0f), Colors.text_subheading, Alignment.LEFT)

        let row = content_bounds.ShrinkT(130.0f).SliceT(35.0f)
        Render.rect (row.SliceL(35.0f).Shrink(10.0f)) Colors.red_accent
        Text.fill_b(Style.font, sprintf "%s: %i" (%"stats.songs_quit") quits, row.ShrinkL(35.0f), Colors.text_subheading, Alignment.LEFT)

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        let completed = completed()
        let retries = retries()
        let quits = quits()

        let total = completed + quits + retries |> max 1

        completed_slider.Target <- float32 completed / float32 total * MathF.PI * 2.0f
        quits_slider.Target <- float32 quits / float32 total * MathF.PI * 2.0f
        retries_slider.Target <- float32 retries / float32 total * MathF.PI * 2.0f

        completed_slider.Update elapsed_ms
        quits_slider.Update elapsed_ms
        retries_slider.Update elapsed_ms