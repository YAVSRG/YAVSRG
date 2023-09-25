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

[<Struct>]
type RenderFrequency =
    | Unlimited
    | Smart
    //| SmartLowLatency

type RenderThread(window: NativeWindow, audioDevice: int, root: Root, afterInit: unit -> unit) =
    
    let mutable resized = false
    let mutable fps_count = 0
    let fps_timer = Stopwatch()
    let last_frame_timer = Stopwatch()
    let total_frame_timer = Stopwatch.StartNew()
    let mutable next_frame_time = 0.0
    let mutable monitor_refresh_period = 1000.0 / 59.997

    let mutable running_sync_test = false
    let mutable sync_samples = ResizeArray<float>()
    let mutable sync_last_interval = 0.0

    member val RenderFrequency = Smart with get, set

    member this.OnResize(newSize: Vector2i) =
        Render.resize(newSize.X, newSize.Y)
        if this.RenderFrequency = Smart then
            running_sync_test <- true
            sync_samples.Clear()
            GLFW.SwapInterval -1
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
        
        // Draw
        // Estimated latency between this frame being drawn and sent to the monitor is calculated
        // Note rendering is adjusted to be further in the future, so it is displaying the right point in time when it arrives on the monitor
        if this.RenderFrequency = Smart && not running_sync_test then 
            Render.Performance.visual_latency <- next_frame_time - total_frame_timer.Elapsed.TotalMilliseconds
        else Render.Performance.visual_latency <- 0.0

        let before_draw = total_frame_timer.Elapsed.TotalMilliseconds
        Render.start()
        if Viewport.rheight > 0 then root.Draw()
        Render.finish()
        let after_draw = total_frame_timer.Elapsed.TotalMilliseconds
        Render.Performance.draw_time <- after_draw - before_draw

        // Frame limiting
        if this.RenderFrequency = Smart && not running_sync_test then

            let now = total_frame_timer.Elapsed.TotalMilliseconds
            let wait_time = next_frame_time - now
            if wait_time > 0.0 then 
                Thread.Sleep(TimeSpan.FromMilliseconds(wait_time))
                next_frame_time <- next_frame_time + monitor_refresh_period
            else
                next_frame_time <- now + monitor_refresh_period / 2.0

        let before_swap = total_frame_timer.Elapsed.TotalMilliseconds
        if not root.ShouldExit then window.Context.SwapBuffers()
        let after_swap = total_frame_timer.Elapsed.TotalMilliseconds

        if running_sync_test then 
            let sample = after_swap - sync_last_interval
            if abs (sample - monitor_refresh_period) < 0.75 * monitor_refresh_period then
                sync_samples.Add sample
            sync_last_interval <- after_swap
            if sync_samples.Count > 600 then
                Logging.Debug(sprintf "True period more like %.5f compared to estimated %.5f" (Seq.average sync_samples) monitor_refresh_period)
                running_sync_test <- false
        
        // Performance profiling
        fps_count <- fps_count + 1
        let time = fps_timer.ElapsedTicks
        if time > 10_000_000L then
            Render.Performance.framecount_tickcount <- (fps_count, time)
            fps_timer.Restart()
            fps_count <- 0
        Render.Performance.swap_time <- after_swap - before_swap
        Render.Performance.elapsed_time <- elapsedTime

    member this.Init() =
        Devices.init audioDevice
        Render.init()
        Render.resize(window.ClientSize.X, window.ClientSize.Y)
        root.Init()
        afterInit()