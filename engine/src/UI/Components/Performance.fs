namespace Percyqaz.Flux.UI

open System.Drawing
open System.Diagnostics
open Percyqaz.Common
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Windowing

type PerformanceMonitor() =
    inherit StaticWidget(NodeType.None)

    let dump_debug = Bind.Key(Keys.F1, (true, true, true))
    let dump_profiling = Bind.Key(Keys.F2, (true, true, true))
    let fps_bind = Bind.Key(Keys.F3, (false, false, false))
    let graph_bind = Bind.Key(Keys.F4, (false, false, false))

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

        if dump_debug.Pressed() then
            Logging.Debug "%s" (Audio.debug_info())
            Logging.Debug "%s" (Render.debug_info())
            WindowThread.defer (fun () ->
                Logging.Debug "%s" (WindowThread.debug_info())
                GameThread.defer (fun () ->
                    failwith "Debug crash, on purpose by pressing the debug dump hotkey"
                )
            )

        if dump_profiling.Pressed() then
            dump_profiling_info ()

        if fps_bind.Pressed() then
            enable <- enable_graph || not enable
            enable_graph <- false

        if graph_bind.Pressed() then
            enable <- not enable_graph || not enable
            enable_graph <- enable

        if enable then

            i <- (i + 599) % 600
            frame_times.[i] <- GameThread.update_draw_elapsed_ms
            draw_times.[i] <- GameThread.draw_time * 2.0
            update_times.[i] <- GameThread.update_time * 10.0
            latencies.[i] <- GameThread.visual_latency_hi

            let (frames, ticks) = GameThread.framecount_tickcount
            fps <- float frames / (float ticks / float Stopwatch.Frequency)

    override this.Draw() =
        if enable then
            Render.rect_c (this.Bounds.SliceL(600.0f)) (Quad.gradient_left_to_right Colors.black.O3 Colors.black.O0)

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
                sprintf "%.1f - %.1fms playfield latency" GameThread.visual_latency_lo GameThread.visual_latency_hi,
                30.0f,
                this.Bounds.Left + 20.0f,
                this.Bounds.Top + 60.0f,
                (Color.White, Color.DarkGreen)
            )

            Text.draw_b (
                Style.font,
                sprintf "%.1fms frame compensation" (GameThread.frame_compensation ()),
                30.0f,
                this.Bounds.Left + 20.0f,
                this.Bounds.Top + 100.0f,
                (Color.White, Color.Black)
            )

            Text.draw_b (
                Style.font,
                sprintf "%.1fms audio compensation" Song.seek_inaccuracy_compensation,
                30.0f,
                this.Bounds.Left + 20.0f,
                this.Bounds.Top + 140.0f,
                (Color.White, Color.Black)
            )

            Text.draw_b (
                Style.font,
                sprintf "%O to show fps" fps_bind,
                30.0f,
                this.Bounds.Left + 20.0f,
                this.Bounds.Top + 190.0f,
                Colors.text_subheading
            )

            Text.draw_b (
                Style.font,
                sprintf "%O to show fps + graph" graph_bind,
                30.0f,
                this.Bounds.Left + 20.0f,
                this.Bounds.Top + 230.0f,
                Colors.text_subheading
            )

        if enable_graph then

            let step = this.Bounds.Width / 600.0f

            let draw_graphs j =
                let x = this.Bounds.Left + (float32 ((j + 600 - i) % 600) * step)

                Render.rect_size
                    x
                    (this.Bounds.Bottom - float32 frame_times.[j])
                    step
                    (float32 frame_times.[j])
                    Color.Red

                Render.rect_size
                    x
                    (this.Bounds.Bottom - 50.0f - float32 draw_times.[j])
                    step
                    (float32 draw_times.[j])
                    Color.Orange

                Render.rect_size
                    x
                    (this.Bounds.Bottom - 100.0f - float32 update_times.[j])
                    step
                    (float32 update_times.[j])
                    Color.Yellow

                Render.rect_size
                    x
                    (this.Bounds.Bottom - 150.0f - float32 latencies.[j])
                    step
                    (float32 latencies.[j])
                    Color.Green

            for a = i to 599 do
                draw_graphs a

            for b = 0 to i - 1 do
                draw_graphs b