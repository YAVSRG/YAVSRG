namespace Percyqaz.Flux.UI

open System.Drawing
open System.Diagnostics
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input

type PerformanceMonitor() =
    inherit StaticWidget(NodeType.None)

    let all_bind = Key (OpenTK.Windowing.GraphicsLibraryFramework.Keys.F3, (true, true, true))
    let graph_bind = Key (OpenTK.Windowing.GraphicsLibraryFramework.Keys.F4, (true, true, true))

    let mutable enable = false
    let mutable enable_graph = false
    let mutable i = 0
    let mutable frame_times = Array.zeroCreate<float> 600
    let mutable draw_times = Array.zeroCreate<float> 600
    let mutable update_times = Array.zeroCreate<float> 600
    let mutable swap_times = Array.zeroCreate<float> 600
    let mutable latencies = Array.zeroCreate<float> 600
    let mutable fps = 0.0

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)

        if all_bind.Tapped() then enable <- not enable; enable_graph <- enable
        if graph_bind.Tapped() then
            if enable then
                enable_graph <- not enable_graph
            else enable <- true

        if enable then
            i <- (i + 599) % 600
            frame_times.[i] <- Render.Performance.elapsed_time
            draw_times.[i] <- Render.Performance.draw_time * 2.0
            update_times.[i] <- Render.Performance.update_time * 10.0
            swap_times.[i] <- Render.Performance.swap_time
            latencies.[i] <- Render.Performance.visual_latency

            let (frames, ticks) = Render.Performance.framecount_tickcount
            fps <- float frames / (float ticks / 10_000_000.0)

    override this.Draw() =
        if enable then
            Text.drawB(Style.font, sprintf "%.3f FPS" fps, 30.0f, this.Bounds.Left + 20.0f, this.Bounds.Top + 20.0f, (Color.White, Color.DarkRed))
            Text.drawB(Style.font, sprintf "%.1fms playfield latency" Render.Performance.visual_latency, 30.0f, this.Bounds.Left + 20.0f, this.Bounds.Top + 60.0f, (Color.White, Color.DarkGreen))
            Text.drawB(Style.font, sprintf "%.1fms UI latency" (Render.Performance.frame_compensation()), 30.0f, this.Bounds.Left + 20.0f, this.Bounds.Top + 100.0f, (Color.White, Color.Black))
            Text.drawB(Style.font, sprintf "%.1fms swap time" Render.Performance.swap_time, 30.0f, this.Bounds.Left + 20.0f, this.Bounds.Top + 140.0f, (Color.White, Color.DarkBlue))
            Text.drawB(Style.font, sprintf "%O to hide overlay" all_bind, 30.0f, this.Bounds.Left + 20.0f, this.Bounds.Top + 180.0f, Colors.text_subheading)
        
        if enable_graph then

            let step = this.Bounds.Width / 600.0f

            let draw_graphs j =
                let x = this.Bounds.Left + (float32 ((j + 600 - i) % 600) * step)
                Draw.rect(Rect.Box(x, this.Bounds.Bottom - float32 frame_times.[j], step, float32 frame_times.[j])) Color.Red
                Draw.rect(Rect.Box(x, this.Bounds.Bottom - 50.0f - float32 draw_times.[j], step, float32 draw_times.[j])) Color.Orange
                Draw.rect(Rect.Box(x, this.Bounds.Bottom - 100.0f - float32 update_times.[j], step, float32 update_times.[j])) Color.Yellow
                Draw.rect(Rect.Box(x, this.Bounds.Bottom - 150.0f - float32 latencies.[j], step, float32 latencies.[j])) Color.Green
                Draw.rect(Rect.Box(x, this.Bounds.Bottom - 200.0f - float32 swap_times.[j], step, float32 swap_times.[j])) Color.Blue

            for a = i to 599 do
                draw_graphs a
            for b = 0 to i - 1 do
                draw_graphs b