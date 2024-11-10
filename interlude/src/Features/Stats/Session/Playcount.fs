namespace Interlude.Features.Stats

open System
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Interlude.UI

type Playcount(plays: unit -> int, completed: unit -> int, retries: unit -> int, quits: unit -> int) =
    inherit StaticWidget(NodeType.None)

    let completed_slider = Animation.Fade(0.0f)
    let retries_slider = Animation.Fade(0.0f)
    let quits_slider = Animation.Fade(0.0f)

    override this.Draw() =
        Draw.rect this.Bounds Colors.shadow_2.O2

        let graph_radius = this.Bounds.Height * 0.4f
        let graph_origin = this.Bounds.Left + this.Bounds.Width * 0.3f

        // todo: empty state when total is 0

        Wedge.draw (graph_origin, this.Bounds.CenterY) (graph_radius - 25.0f) graph_radius 
            0.0 (float completed_slider.Value) 
            Colors.green_accent
        Wedge.draw (graph_origin, this.Bounds.CenterY) (graph_radius - 25.0f) graph_radius 
            (float completed_slider.Value) (float completed_slider.Value + float retries_slider.Value)
            Colors.yellow_accent
        Wedge.draw (graph_origin, this.Bounds.CenterY) (graph_radius - 25.0f) graph_radius
            (float completed_slider.Value + float retries_slider.Value) (float completed_slider.Value + float retries_slider.Value + float quits_slider.Value)
            Colors.red_accent
            
        let plays = plays()
        let completed = completed()
        let retries = retries()
        let quits = quits()

        Text.draw_b(Style.font, sprintf "Songs played: %i" plays, 28.0f, this.Bounds.CenterX, this.Bounds.Top + 25.0f, Colors.text)

        Draw.rect (Rect.Box(this.Bounds.CenterX, this.Bounds.Top + 85.0f, 10.0f, 10.0f)) Colors.green_accent
        Text.draw_b(Style.font, sprintf "Songs completed: %i" completed, 20.0f, this.Bounds.CenterX + 20.0f, this.Bounds.Top + 75.0f, Colors.text_subheading)

        Draw.rect (Rect.Box(this.Bounds.CenterX, this.Bounds.Top + 115.0f, 10.0f, 10.0f)) Colors.yellow_accent
        Text.draw_b(Style.font, sprintf "Songs retried: %i" retries, 20.0f, this.Bounds.CenterX + 20.0f, this.Bounds.Top + 105.0f, Colors.text_subheading)

        Draw.rect (Rect.Box(this.Bounds.CenterX, this.Bounds.Top + 145.0f, 10.0f, 10.0f)) Colors.red_accent
        Text.draw_b(Style.font, sprintf "Songs quit/failed: %i" quits, 20.0f, this.Bounds.CenterX + 20.0f, this.Bounds.Top + 135.0f, Colors.text_subheading)
    
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