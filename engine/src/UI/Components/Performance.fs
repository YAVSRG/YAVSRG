namespace Percyqaz.Flux.UI

open System.Drawing
open System.Diagnostics
open OpenTK.Windowing.GraphicsLibraryFramework
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input

type PerformanceMonitor() =
    inherit StaticWidget(NodeType.None)

    let all_bind = Key(Keys.F3, (true, true, true))
    let graph_bind = Key(Keys.F4, (true, true, true))

    let mutable enable = false
    let mutable enable_graph = false
    let mutable i = 0
    let mutable frame_times = Array.zeroCreate<float> 600
    let mutable draw_times = Array.zeroCreate<float> 600
    let mutable update_times = Array.zeroCreate<float> 600
    let mutable latencies = Array.zeroCreate<float> 600
    let mutable fps = 0.0

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if all_bind.Tapped() then
            enable <- not enable
            enable_graph <- enable

        if graph_bind.Tapped() then
            if enable then
                enable_graph <- not enable_graph
            else
                enable <- true

        if enable then
            i <- (i + 599) % 600
            frame_times.[i] <- Render.Performance.elapsed_ms
            draw_times.[i] <- Render.Performance.draw_time * 2.0
            update_times.[i] <- Render.Performance.update_time * 10.0
            latencies.[i] <- Render.Performance.visual_latency_hi

            let (frames, ticks) = Render.Performance.framecount_tickcount
            fps <- float frames / (float ticks / float Stopwatch.Frequency)

    override this.Draw() =
        if enable then
            Text.draw_b (
                Style.font,
                sprintf "%.3f FPS" fps,
                30.0f,
                this.Bounds.Left + 20.0f,
                this.Bounds.Top + 20.0f,
                (Color.White, Color.DarkRed)
            )

            Text.draw_b (
                Style.font,
                sprintf
                    "%.1f - %.1fms playfield latency"
                    Render.Performance.visual_latency_lo
                    Render.Performance.visual_latency_hi,
                30.0f,
                this.Bounds.Left + 20.0f,
                this.Bounds.Top + 60.0f,
                (Color.White, Color.DarkGreen)
            )

            Text.draw_b (
                Style.font,
                sprintf "%.1fms frame compensation" (Render.Performance.frame_compensation ()),
                30.0f,
                this.Bounds.Left + 20.0f,
                this.Bounds.Top + 100.0f,
                (Color.White, Color.Black)
            )

            Text.draw_b (
                Style.font,
                sprintf "%O to hide overlay" all_bind,
                30.0f,
                this.Bounds.Left + 20.0f,
                this.Bounds.Top + 150.0f,
                Colors.text_subheading
            )
            
            Text.draw_b (
                Style.font,
                sprintf "%O to toggle graph" graph_bind,
                30.0f,
                this.Bounds.Left + 20.0f,
                this.Bounds.Top + 190.0f,
                Colors.text_subheading
            )

        if enable_graph then

            let step = this.Bounds.Width / 600.0f

            let draw_graphs j =
                let x = this.Bounds.Left + (float32 ((j + 600 - i) % 600) * step)

                Draw.rect
                    (Rect.Box(x, this.Bounds.Bottom - float32 frame_times.[j], step, float32 frame_times.[j]))
                    Color.Red

                Draw.rect
                    (Rect.Box(x, this.Bounds.Bottom - 50.0f - float32 draw_times.[j], step, float32 draw_times.[j]))
                    Color.Orange

                Draw.rect
                    (Rect.Box(x, this.Bounds.Bottom - 100.0f - float32 update_times.[j], step, float32 update_times.[j]))
                    Color.Yellow

                Draw.rect
                    (Rect.Box(x, this.Bounds.Bottom - 150.0f - float32 latencies.[j], step, float32 latencies.[j]))
                    Color.Green

            for a = i to 599 do
                draw_graphs a

            for b = 0 to i - 1 do
                draw_graphs b
