namespace Percyqaz.Flux.UI

open System.Drawing
open System.Diagnostics
open Percyqaz.Common
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Windowing

type DebugGraph =
    {
        Color: Color
        mutable IsEnabled: bool
        IsDuringDraw: bool
        Func: unit -> float
        Data: float array
    }

    static member Create(color: Color, func: unit -> float) : DebugGraph =
        {
            Color = color
            IsEnabled = false
            Data = Array.zeroCreate<float> 600
            Func = func
            IsDuringDraw = false
        }

    member this.Enabled() : DebugGraph = { this with IsEnabled = true }
    member this.DuringDraw() : DebugGraph = { this with IsDuringDraw = true }

type DebugOverlay() =
    inherit StaticWidget(NodeType.None)

    let dump_debug = Bind.Key(Keys.F1, (true, true, true))
    let dump_profiling = Bind.Key(Keys.F2, (true, true, true))
    let fps_bind = Bind.Key(Keys.F3, (false, false, false))
    let graph_bind = Bind.Key(Keys.F4, (false, false, false))

    let mutable enable = false
    let mutable enable_graph = false
    let mutable i = 0

    let graphs =
        [|
            DebugGraph.Create(Color.Red, fun () -> GameThread.update_draw_elapsed_ms * 2.0).Enabled()
            DebugGraph.Create(Color.Orange, fun () -> GameThread.draw_time * 2.0).Enabled()
            DebugGraph.Create(Color.Yellow, fun () -> GameThread.update_time * 2.0).Enabled()
            DebugGraph.Create(Color.Green, fun () -> GameThread.visual_latency_hi)
            DebugGraph.Create(Color.Blue.O2, fun () -> float(Song.DebugTimelineDrift())).Enabled()
            DebugGraph.Create(Color.Purple.O2, fun () -> float(Song.FrameCompensation) * 4.0).DuringDraw().Enabled()
        |]

    let mutable fps = 0.0
    let mutable pps = 0.0
    let mutable pps_count = 0
    let mutable pps_ts = 0L

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if dump_debug.Pressed() then
            Logging.Debug "%s" (Audio.debug_info())
            Logging.Debug "%s" (Render.debug_info())

            WindowThread.defer(fun () ->
                Logging.Debug "%s" (WindowThread.debug_info())
                GameThread.defer(fun () -> failwith "Debug crash, on purpose by pressing the debug dump hotkey")
            )

        if dump_profiling.Pressed() then
            dump_profiling_info()

        if fps_bind.Pressed() then
            enable <- enable_graph || not enable
            enable_graph <- false

        if graph_bind.Pressed() then
            enable <- not enable_graph || not enable
            enable_graph <- enable

        if enable then
            i <- (i + 599) % 600

            for graph in graphs do
                if not graph.IsDuringDraw then
                    graph.Data.[i] <- graph.Func()

            let frames, ticks = GameThread.framecount_tickcount
            fps <- float frames / (float ticks / float Stopwatch.Frequency)

        pps_count <- pps_count + Input.this_frame.Polls
        let elapsed = Stopwatch.GetElapsedTime(pps_ts)

        if elapsed.TotalSeconds >= 1.0 then
            pps <- float pps_count / elapsed.TotalSeconds
            pps_count <- 0
            pps_ts <- Stopwatch.GetTimestamp()

    override this.Draw() =
        if enable then
            Render.rect_c (this.Bounds.SliceL(600.0f)) (Quad.gradient_left_to_right Colors.black.O3 Colors.black.O0)

            let mutable offset = 20.0f

            let debug_text (text: string, color: Color) =
                let x = this.Bounds.Left + 20.0f
                let y = this.Bounds.Top + offset
                Text.draw_b(Style.font, text, 30.0f, x, y, (Color.White, color))
                offset <- offset + 40.0f

            debug_text(sprintf "FPS: %.3f" fps, Color.DarkRed)
            debug_text(sprintf "Input polls: %.3f" pps, Color.Black)

            debug_text(
                sprintf "playfield latency: %.1f - %.1fms" GameThread.visual_latency_lo GameThread.visual_latency_hi,
                Color.DarkGreen
            )

            debug_text(sprintf "frame compensation: %.1fms" (GameThread.frame_compensation()), Color.Black)
            debug_text(sprintf "audio comp. (actual): %.1fms" Song.DebugCompensationAtStartOfFrame, Color.Black)
            debug_text(sprintf "audio comp. (target): %.1fms" Song.DebugActualBassDelay, Color.Black)
            debug_text(sprintf "audio drift: %.1fms" (Song.DebugTimelineDrift()), Color.Blue)

            Text.draw_aligned_b(
                Style.font,
                sprintf "%O to show fps" fps_bind,
                30.0f,
                this.Bounds.Right - 20.0f,
                this.Bounds.Top + 20.0f,
                Colors.text_subheading,
                Alignment.RIGHT
            )

            Text.draw_aligned_b(
                Style.font,
                sprintf "%O to show fps + graph" graph_bind,
                30.0f,
                this.Bounds.Right - 20.0f,
                this.Bounds.Top + 60.0f,
                Colors.text_subheading,
                Alignment.RIGHT
            )

        if enable_graph then

            let step = this.Bounds.Width / 600.0f

            let draw_graphs j =
                let x = this.Bounds.Left + (float32((j + 600 - i) % 600) * step)

                let mutable offset = 5.0f

                for graph in graphs do
                    if graph.IsEnabled then
                        Render.rect_size
                            x
                            (this.Bounds.Bottom - offset - float32 graph.Data.[j])
                            step
                            (float32 graph.Data.[j])
                            graph.Color

                        offset <- offset + 50.0f

            for graph in graphs do
                if graph.IsEnabled && graph.IsDuringDraw then
                    graph.Data.[i] <- graph.Func()

            for a = i to 599 do
                draw_graphs a

            for b = 0 to i - 1 do
                draw_graphs b
