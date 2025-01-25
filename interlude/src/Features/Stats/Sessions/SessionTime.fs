namespace Interlude.Features.Stats

open System
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Data.User.Stats
open Interlude.UI

type SessionTime(total_time: unit -> float, play_time: unit -> float, practice_time: unit -> float) =
    inherit StaticWidget(NodeType.None)

    let total_slider = Animation.Fade(0.0f, Target = MathF.PI * 2.0f)
    let play_slider = Animation.Fade(0.0f)
    let practice_slider = Animation.Fade(0.0f)

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

        Wedge.draw (graph_origin, this.Bounds.CenterY) (graph_radius - graph_thickness) graph_radius 0.0 (float total_slider.Value) Colors.grey_2
        Wedge.draw (graph_origin, this.Bounds.CenterY) (graph_radius - graph_thickness) graph_radius 0.0 (float play_slider.Value) Colors.cyan_accent
        Wedge.draw (graph_origin, this.Bounds.CenterY) (graph_radius - graph_thickness) graph_radius (float play_slider.Value) (float play_slider.Value + float practice_slider.Value) Colors.green_accent

        let total = total_time()
        let play = play_time()
        let practice = practice_time()
        let other = total - play - practice |> max 0.0

        Text.fill_b(Style.font, sprintf "%s: %s" (%"stats.session_time") (format_short_time total), content_bounds.SliceT(45.0f), Colors.text, Alignment.LEFT)

        let row = content_bounds.ShrinkT(50.0f).SliceT(35.0f)
        Render.rect (row.SliceL(35.0f).Shrink(10.0f)) Colors.cyan_accent
        Text.fill_b(Style.font, sprintf "%s: %s" (%"stats.time_ingame") (format_short_time play), row.ShrinkL(35.0f), Colors.text_subheading, Alignment.LEFT)

        let row = content_bounds.ShrinkT(90.0f).SliceT(35.0f)
        Render.rect (row.SliceL(35.0f).Shrink(10.0f)) Colors.green_accent
        Text.fill_b(Style.font, sprintf "%s: %s" (%"stats.time_practice") (format_short_time practice), row.ShrinkL(35.0f), Colors.text_subheading, Alignment.LEFT)

        let row = content_bounds.ShrinkT(130.0f).SliceT(35.0f)
        Render.rect (row.SliceL(35.0f).Shrink(10.0f)) Colors.grey_2
        Text.fill_b(Style.font, sprintf "%s: %s" (%"stats.time_other") (format_short_time other), row.ShrinkL(35.0f), Colors.text_subheading, Alignment.LEFT)

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        let total = total_time() + 0.1
        play_slider.Target <- float32 (play_time() / total) * MathF.PI * 2.0f
        practice_slider.Target <- float32 (practice_time() / total) * MathF.PI * 2.0f

        total_slider.Update elapsed_ms
        play_slider.Update elapsed_ms
        practice_slider.Update elapsed_ms