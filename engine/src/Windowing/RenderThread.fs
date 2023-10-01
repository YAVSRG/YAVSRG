namespace Percyqaz.Flux.Windowing

open System
open System.Threading
open System.Diagnostics
open System.Runtime.InteropServices
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

module Timing =

    [<DllImport("winmm.dll")>]
    extern uint private timeBeginPeriod(int msec)
    [<DllImport("winmm.dll")>]
    extern uint private timeEndPeriod(int msec)

    let NATIVE_SLEEP_TRUST_THRESHOLD_MS =
        if OperatingSystem.IsWindows() then 1.0 else 0.125

    let spin_wait = SpinWait()

    /// Sleep the thread until a stopwatch time is reached
    /// Strategy is to natively sleep the thread as far as it can be trusted, then spin-wait until it's time
    /// Native sleep can still wake up much later if threads are busy
    /// CPU intensive
    let sleep_accurate : Stopwatch * float -> unit =
        if OperatingSystem.IsWindows() then

            fun (timer: Stopwatch, until: float) ->
                let delta = until - timer.Elapsed.TotalMilliseconds

                if delta > NATIVE_SLEEP_TRUST_THRESHOLD_MS then
                    timeBeginPeriod 1 |> ignore
                    Thread.Sleep(TimeSpan.FromMilliseconds (delta - NATIVE_SLEEP_TRUST_THRESHOLD_MS))
                    timeEndPeriod 1 |> ignore

                while timer.Elapsed.TotalMilliseconds < until do
                    spin_wait.SpinOnce -1

        else

            fun (timer: Stopwatch, until: float) ->
                let delta = until - timer.Elapsed.TotalMilliseconds

                if delta > NATIVE_SLEEP_TRUST_THRESHOLD_MS then
                    Thread.Sleep(TimeSpan.FromMilliseconds (delta - NATIVE_SLEEP_TRUST_THRESHOLD_MS))

                while timer.Elapsed.TotalMilliseconds < until do
                    spin_wait.SpinOnce -1


type RenderThread(window: NativeWindow, audioDevice: int, root: Root, afterInit: unit -> unit) =
    
    let mutable resized = false
    let mutable fps_count = 0
    let fps_timer = Stopwatch()
    let last_frame_timer = Stopwatch()
    let total_frame_timer = Stopwatch.StartNew()
    let mutable next_frame_time = 0.0
    let mutable refresh_period = 1000.0 / 60.0
    let mutable vsync = false

    let VSYNC_FAIL_THRESHOLD = 3
    let mutable vsync_fails = 0

    member val RenderMode = FrameLimit.Smart with get, set

    member this.OnResize(newSize: Vector2i) =
        Render.resize(newSize.X, newSize.Y)
        resized <- true

    member this.RenderModeChanged(refresh_rate: int) =
        refresh_period <- 1000.0 / float refresh_rate
        vsync <- this.RenderMode = FrameLimit.Smart
        vsync_fails <- 0
        GLFW.SwapInterval (if vsync then -1 else 0)

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

        let elapsed_time = last_frame_timer.Elapsed.TotalMilliseconds
        last_frame_timer.Restart()
        
        // Update
        let before_update = total_frame_timer.Elapsed.TotalMilliseconds
        Input.begin_frame_events()
        ROOT_ANIMATION.Update elapsed_time
        root.Update (elapsed_time, resized)
        resized <- false
        Input.finish_frame_events()
        Devices.update elapsed_time
        let after_update = total_frame_timer.Elapsed.TotalMilliseconds
        Render.Performance.update_time <- after_update - before_update
        if root.ShouldExit then window.Close()
        
        // Draw
        let before_draw = total_frame_timer.Elapsed.TotalMilliseconds
        Render.start()
        if Viewport.rheight > 0 then root.Draw()
        Render.finish()
        let after_draw = total_frame_timer.Elapsed.TotalMilliseconds
        Render.Performance.draw_time <- after_draw - before_draw

        // Frame limiting
        if this.RenderMode = FrameLimit.Smart && not vsync then
            Timing.sleep_accurate(total_frame_timer, next_frame_time)

        let before_swap = total_frame_timer.Elapsed.TotalMilliseconds
        if not root.ShouldExit then window.Context.SwapBuffers()
        let after_swap = total_frame_timer.Elapsed.TotalMilliseconds
        Render.Performance.swap_time <- after_swap - before_swap

        if this.RenderMode = FrameLimit.Smart then 
            Render.Performance.visual_latency <- after_swap - next_frame_time
        else Render.Performance.visual_latency <- 0.0

        if vsync then
            next_frame_time <- after_swap + refresh_period
        else
            while next_frame_time < after_swap do
                next_frame_time <- next_frame_time + refresh_period
        
        // Performance profiling
        fps_count <- fps_count + 1
        let time = fps_timer.ElapsedTicks
        if time > Stopwatch.Frequency then
            Render.Performance.framecount_tickcount <- (fps_count, time)

            // Automatically turn off vsync when it is doing the wrong thing, switch to software timer
            if vsync then
                let actual_fps = float fps_count / (float time / float Stopwatch.Frequency)
                let expected_fps = 1000.0 / refresh_period
                if abs (actual_fps - expected_fps) > 5.0 then vsync_fails <- vsync_fails + 1
                if vsync_fails >= VSYNC_FAIL_THRESHOLD then
                    Logging.Debug(sprintf "Turning off VSync: Was giving %.1f FPS but expected was %.1f FPS" actual_fps expected_fps)
                    vsync <- false
                    GLFW.SwapInterval 0

            fps_timer.Restart()
            fps_count <- 0
        Render.Performance.elapsed_time <- elapsed_time

    member this.Init() =
        Devices.init audioDevice
        Render.init()
        Render.resize(window.ClientSize.X, window.ClientSize.Y)
        Render.Performance.frame_compensation <- 
            fun () -> 
                if this.RenderMode = FrameLimit.Smart then 
                    float32 (refresh_period + next_frame_time - total_frame_timer.Elapsed.TotalMilliseconds) * 1.0f<ms>
                else 0.0f<ms>
        root.Init()
        afterInit()