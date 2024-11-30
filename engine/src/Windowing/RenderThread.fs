namespace Percyqaz.Flux.Windowing

open System
open System.Threading
open System.Diagnostics
open OpenTK.Windowing.Desktop
open OpenTK.Windowing.GraphicsLibraryFramework
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Percyqaz.Common

type UIEntryPoint =
    abstract member ShouldExit: bool
    abstract member Init: unit -> unit
    abstract member Update: float * bool -> unit
    abstract member Draw: unit -> unit
            
type Strategy =
    | Unlimited
    | WindowsDwmFlush
    | WindowsVblankSync

[<AutoOpen>]
module SmartCapConstants =
    
    let mutable no_compositor = false
    let mutable anti_jitter = false
    let mutable tearline_position = 0.75
    let mutable framerate_multiplier = 8.0

module RenderThread =

    let mutable private window: nativeptr<Window> = Unchecked.defaultof<_>
    let mutable private ui_root: UIEntryPoint = Unchecked.defaultof<_>

    let private LOCK_OBJ = obj()
    
    (*
        Action queuing
        
        Most of the game runs from the 'render thread' where draws and updates take place
        `defer` can be used to queue up an action to be executed before the next frame update
        Used for: 
        - Queuing actions to take place on this thread from other threads
        - Deferring an action to be done at the start of the next frame for other logic/UI reasons

        Deferred actions are fire-and-forget, they will execute in the order they are queued
    *)

    let mutable internal UI_THREAD_ID = -1
    let is_ui_thread() =
        UI_THREAD_ID = -1 || Thread.CurrentThread.ManagedThreadId = UI_THREAD_ID

    let mutable private action_queue : (unit -> unit) list = []
    let private run_action_queue() =
        lock (LOCK_OBJ) (fun () -> (for action in action_queue do action()); action_queue <- [])
    let defer (action: unit -> unit) =
        lock (LOCK_OBJ) (fun () -> action_queue <- action_queue @ [ action ])

    let inline ensure_ui_thread (action: unit -> unit) =
        if is_ui_thread () then action () else defer action

    (*
        Events

        Triggers for an application to hook into for initialisation and shutdown logic
    *)

    let after_init_ev = Event<unit>()
    let after_init = after_init_ev.Publish

    (*
        
    *)

    let mutable resized = false
    let mutable fps_count = 0
    let fps_timer = Stopwatch()
    let last_frame_timer = Stopwatch()
    let total_frame_timer = Stopwatch.StartNew()
    let mutable estimated_next_frame = 0.0
    let mutable real_next_frame = 0.0
    let mutable start_of_frame = 0.0
    let mutable frame_is_ready = 0.0
    let mutable strategy = Unlimited

    let private now () = total_frame_timer.Elapsed.TotalMilliseconds

    let mutable private fatal_error = false
    let has_fatal_error () =
        fatal_error

    let viewport_resized(width, height) =
        assert(is_ui_thread())
        Render.viewport_resized (width, height)
        resized <- true

    let change_mode (frame_limit: FrameLimit, refresh_rate: int, entire_monitor: bool, monitor: nativeptr<Monitor>) =
        assert(is_ui_thread())
        no_compositor <- entire_monitor
        strategy <-
            if OperatingSystem.IsWindows() then
                // On windows:
                //  Smart = Custom frame pacing strategies
                //  Unlimited = Unlimited
                GLFW.SwapInterval(0)
                if frame_limit = FrameLimit.Smart then
                    FrameTimeStrategies.VBlankThread.switch (1000.0 / float refresh_rate) (GLFW.GetWin32Adapter monitor) (GLFW.GetWin32Monitor monitor)
                    if entire_monitor then WindowsVblankSync else WindowsDwmFlush
                else
                    Unlimited
            else
                // On non-windows:
                //  Smart = GLFW's default Vsync
                //  Unlimited = Unlimited
                if frame_limit = FrameLimit.Smart then
                    GLFW.SwapInterval(1)
                    Unlimited
                else
                    GLFW.SwapInterval(0)
                    Unlimited

    let private dispatch_frame() =

        Performance.visual_latency_lo <- frame_is_ready - real_next_frame
        Performance.visual_latency_hi <- start_of_frame - real_next_frame

        match strategy with

        | Unlimited -> ()

        | WindowsVblankSync ->
            let _, last_vblank, est_refresh_period = FrameTimeStrategies.VBlankThread.get(tearline_position, total_frame_timer)
            estimated_next_frame <- last_vblank + est_refresh_period

            let time_taken_to_render = frame_is_ready - start_of_frame
            FrameTimeStrategies.sleep_accurate (total_frame_timer, now() - time_taken_to_render + est_refresh_period / framerate_multiplier)
        
        | WindowsDwmFlush ->
            FrameTimeStrategies.DwmFlush() |> ignore

            let _, last_vblank, est_refresh_period = FrameTimeStrategies.VBlankThread.get(tearline_position, total_frame_timer)
            estimated_next_frame <- last_vblank + est_refresh_period

        let elapsed_ms = last_frame_timer.Elapsed.TotalMilliseconds
        last_frame_timer.Restart()

        // Update
        start_of_frame <- now ()
        Input.begin_frame_events ()
        run_action_queue()
        ui_root.Update(elapsed_ms, resized)
        resized <- false
        Input.finish_frame_events ()
        Devices.update elapsed_ms
        Performance.update_time <- now () - start_of_frame

        if ui_root.ShouldExit then
            GLFW.SetWindowShouldClose(window, true)

        // Draw
        let before_draw = now ()
        Render.start ()

        if Render._viewport_height > 0 then
            ui_root.Draw()

        Render.finish ()
        frame_is_ready <- now ()
        Performance.draw_time <- frame_is_ready - before_draw

        if not ui_root.ShouldExit then
            GLFW.SwapBuffers(window)
            real_next_frame <- now ()

        // Performance profiling
        fps_count <- fps_count + 1
        let time = fps_timer.ElapsedTicks

        if time > Stopwatch.Frequency then
            Performance.framecount_tickcount <- (fps_count, time)
            fps_timer.Restart()
            fps_count <- 0

        Performance.elapsed_ms <- elapsed_ms

    let private main_loop () =
        GLFW.MakeContextCurrent(window)
        let width, height = GLFW.GetFramebufferSize(window)
        if width = 0 || height = 0 then Render.DEFAULT_SCREEN else (width, height)
        |> Render.init

        if OperatingSystem.IsWindows() then FrameTimeStrategies.VBlankThread.start total_frame_timer

        ui_root.Init()
        after_init_ev.Trigger()
        fps_timer.Start()

        try
            Console.hide()
            while not (GLFW.WindowShouldClose window) do
                dispatch_frame()
        with fatal_err ->
            fatal_error <- true
            Logging.Critical("Fatal crash in UI thread", fatal_err)
            Console.restore()
            GLFW.SetWindowShouldClose(window, true)

        if OperatingSystem.IsWindows() then FrameTimeStrategies.VBlankThread.stop ()

    let private thread = Thread(main_loop)

    let internal init(_window: nativeptr<Window>, _ui_root: UIEntryPoint, audio_device: int, audio_device_period: int, audio_device_buffer_length: int) =
        UI_THREAD_ID <- thread.ManagedThreadId
        window <- _window
        ui_root <- _ui_root

        Devices.init(audio_device, audio_device_period, audio_device_buffer_length)

        Performance.frame_compensation <-
            fun () ->
                if strategy <> Unlimited && anti_jitter then
                    float32 (estimated_next_frame - now ()) * 1.0f<ms / rate>
                else
                    0.0f<ms / rate>

    let internal start() =
        thread.Start()