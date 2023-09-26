namespace Percyqaz.Flux.Windowing

open System
open System.Threading
open System.Diagnostics
open OpenTK
open OpenTK.Mathematics
open OpenTK.Windowing.Desktop
open OpenTK.Windowing.Common
open OpenTK.Windowing.GraphicsLibraryFramework
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Common

type RenderThread(window: NativeWindow, audioDevice: int, root: Root, afterInit: unit -> unit) =
    
    let mutable resized = false
    let mutable fps_count = 0
    let fps_timer = Stopwatch()
    let last_frame_timer = Stopwatch()
    let total_frame_timer = Stopwatch.StartNew()
    let mutable next_frame_time = 0.0
    let mutable monitor_refresh_period = 1000.0 / 60.0
    let mutable vsync = false

    member val RenderMode = FrameLimit.Smart with get, set

    member this.OnResize(newSize: Vector2i, refresh_rate: int) =
        Render.resize(newSize.X, newSize.Y)
        monitor_refresh_period <- 1000.0 / float refresh_rate
        GLFW.SwapInterval (if vsync then -1 else 0)
        resized <- true

    member private this.Loop() =
        window.Context.MakeCurrent()
        this.Init()
        fps_timer.Start()
        try
            while not (GLFW.WindowShouldClose window.WindowPtr) do this.DispatchFrame()
        with fatal_err -> Logging.Critical("Fatal crash in UI thread", fatal_err); window.Close()

    member this.Start() =
        let thread = Thread(this.Loop)
        Percyqaz.Flux.Utils.UITHREAD <- thread.ManagedThreadId
        thread.Start()
        
    member this.DispatchFrame() =

        let elapsedTime = last_frame_timer.Elapsed.TotalMilliseconds
        last_frame_timer.Restart()
        
        // Update
        let before_update = total_frame_timer.Elapsed.TotalMilliseconds
        Input.begin_frame_events()
        ROOT_ANIMATION.Update elapsedTime
        root.Update (elapsedTime, resized)
        resized <- false
        Input.finish_frame_events()
        Devices.update(elapsedTime)
        let after_update = total_frame_timer.Elapsed.TotalMilliseconds
        Render.Performance.update_time <- after_update - before_update
        if root.ShouldExit then window.Close()

        if vsync <> (this.RenderMode = FrameLimit.Smart) then
            vsync <- this.RenderMode = FrameLimit.Smart
            GLFW.SwapInterval (if vsync then -1 else 0)
        
        // Draw
        // Estimated latency between this frame being drawn and sent to the monitor is calculated
        // Note rendering is adjusted to be further in the future, so it is displaying the right point in time when it arrives on the monitor

        let before_draw = total_frame_timer.Elapsed.TotalMilliseconds
        Render.start()
        if Viewport.rheight > 0 then root.Draw()
        Render.finish()
        let after_draw = total_frame_timer.Elapsed.TotalMilliseconds
        Render.Performance.draw_time <- after_draw - before_draw

        // Frame limiting
        let before_swap = total_frame_timer.Elapsed.TotalMilliseconds
        if not root.ShouldExit then window.Context.SwapBuffers()
        let after_swap = total_frame_timer.Elapsed.TotalMilliseconds
        Render.Performance.swap_time <- after_swap - before_swap

        if this.RenderMode = FrameLimit.Smart then 
            Render.Performance.visual_latency <- after_swap - next_frame_time
        else Render.Performance.visual_latency <- 0.0

        next_frame_time <- after_swap + monitor_refresh_period
        
        // Performance profiling
        fps_count <- fps_count + 1
        let time = fps_timer.ElapsedTicks
        if time > 10_000_000L then
            Render.Performance.framecount_tickcount <- (fps_count, time)
            fps_timer.Restart()
            fps_count <- 0
        Render.Performance.elapsed_time <- elapsedTime

    member this.Init() =
        Devices.init audioDevice
        Render.init()
        Render.resize(window.ClientSize.X, window.ClientSize.Y)
        Render.Performance.frame_compensation <- 
            fun () -> 
                if this.RenderMode = FrameLimit.Smart then 
                    float32 (next_frame_time - total_frame_timer.Elapsed.TotalMilliseconds) * 1.0f<ms>
                else 0.0f<ms>
        root.Init()
        afterInit()