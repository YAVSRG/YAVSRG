namespace Interlude.Features.Stats

open System
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude.Data.User
open Interlude.UI

type Playtime(total_time: unit -> float, play_time: unit -> float, practice_time: unit -> float) =
    inherit StaticWidget(NodeType.None)

    let total_slider = Animation.Fade(0.0f, Target = MathF.PI * 2.0f)
    let play_slider = Animation.Fade(0.0f)
    let practice_slider = Animation.Fade(0.0f)

    override this.Draw() =
        Draw.rect this.Bounds Colors.shadow_2.O2

        let graph_radius = this.Bounds.Height * 0.4f
        let graph_origin = this.Bounds.Left + this.Bounds.Width * 0.3f

        Wedge.draw (graph_origin, this.Bounds.CenterY) (graph_radius - 25.0f) graph_radius 0.0 (float total_slider.Value) Colors.grey_2
        Wedge.draw (graph_origin, this.Bounds.CenterY) (graph_radius - 25.0f) graph_radius (0.0) (float play_slider.Value) Colors.cyan_accent
        Wedge.draw (graph_origin, this.Bounds.CenterY) (graph_radius - 25.0f) graph_radius (float play_slider.Value) (float play_slider.Value + float practice_slider.Value) Colors.green_accent

        let total = total_time()
        let play = play_time()
        let practice = practice_time()
        let other = total - play - practice

        // todo: when global stats use `format_long_time`
        Text.draw_b(Style.font, "Session time: " + Stats.format_short_time total, 28.0f, this.Bounds.CenterX, this.Bounds.Top + 25.0f, Colors.text)

        Draw.rect (Rect.Box(this.Bounds.CenterX, this.Bounds.Top + 85.0f, 10.0f, 10.0f)) Colors.cyan_accent
        Text.draw_b(Style.font, "Play time: " + Stats.format_short_time play, 20.0f, this.Bounds.CenterX + 20.0f, this.Bounds.Top + 75.0f, Colors.text_subheading)

        Draw.rect (Rect.Box(this.Bounds.CenterX, this.Bounds.Top + 115.0f, 10.0f, 10.0f)) Colors.green_accent
        Text.draw_b(Style.font, "Practice time: " + Stats.format_short_time practice, 20.0f, this.Bounds.CenterX + 20.0f, this.Bounds.Top + 105.0f, Colors.text_subheading)

        Draw.rect (Rect.Box(this.Bounds.CenterX, this.Bounds.Top + 145.0f, 10.0f, 10.0f)) Colors.grey_2
        Text.draw_b(Style.font, "Other: " + Stats.format_short_time other, 20.0f, this.Bounds.CenterX + 20.0f, this.Bounds.Top + 135.0f, Colors.text_subheading)
    
    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        let total = total_time() + 0.1
        play_slider.Target <- float32 (play_time() / total) * MathF.PI * 2.0f
        practice_slider.Target <- float32 (practice_time() / total) * MathF.PI * 2.0f

        total_slider.Update elapsed_ms
        play_slider.Update elapsed_ms
        practice_slider.Update elapsed_ms