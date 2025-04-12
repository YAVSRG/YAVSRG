namespace Percyqaz.Flux.Windowing

open System
open System.Threading
open System.Diagnostics
open OpenTK.Windowing.GraphicsLibraryFramework
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Percyqaz.Common

type UIEntryPoint =
    abstract member Init: unit -> unit
    abstract member Update: float * bool -> unit
    abstract member Draw: unit -> unit

type private Strategy =
    | Unlimited
    | FrameCap of frame_time: float
    | WindowsDwmFlush
    | WindowsVblankSync

module GameThread =

    let mutable private window: nativeptr<Window> = Unchecked.defaultof<_>
    let mutable private ui_initialiser: unit -> UIEntryPoint = fun () -> Unchecked.defaultof<_>
    let mutable private loading_icon: Bitmap option = None

    let ACTION_QUEUE = ThreadActionQueue()
    let is_game_thread() = ACTION_QUEUE.IsCurrent()
    let defer (action: unit -> unit) : unit = ACTION_QUEUE.Defer action
    let on_game_thread (action: unit -> unit) : unit = ACTION_QUEUE.EnsureCurrent action

    let private after_init_ev = Event<unit>()
    let after_init = after_init_ev.Publish

    (*
        Timer variables
    *)

    let mutable private resized = false
    let mutable private fps_count = 0
    let private fps_timer = Stopwatch()
    let private last_frame_timer = Stopwatch()
    let private total_frame_timer = Stopwatch.StartNew()
    let mutable private estimated_next_frame = 0.0
    let mutable private real_next_frame = 0.0
    let mutable private start_of_frame = 0.0
    let mutable private frame_is_ready = 0.0
    let mutable private strategy = Unlimited
    let mutable private looping = true

    let private now () = total_frame_timer.Elapsed.TotalMilliseconds

    (*
        Global variables (to be refactored)
        These can be read from the game thread
    *)

    let mutable uses_compositor = false
    let mutable anti_jitter = false
    let mutable tearline_position = 0.75
    let mutable framerate_multiplier = 8.0
    let mutable framecount_tickcount = (0, 1L)
    let mutable visual_latency_lo = 0.0
    let mutable visual_latency_hi = 0.0
    let mutable update_time = 0.0
    let mutable draw_time = 0.0
    let mutable update_draw_elapsed_ms = 0.0

    let frame_compensation () =
        if strategy <> Unlimited && anti_jitter && Song.playing() then
            let audio_cmp = Song.frame_compensation() / Song.playback_rate()
            let visual_cmp = float32 (estimated_next_frame - now ()) * 1.0f<ms / rate>
            audio_cmp + visual_cmp
        else
            0.0f<ms / rate>

    (*
        Main loop
    *)

    let mutable private fatal_error = false
    let has_fatal_error () =
        fatal_error

    let internal framebuffer_resized framebuffer viewport =
        assert(is_game_thread())
        Render.framebuffer_resized framebuffer viewport
        resized <- true

    let internal change_mode (frame_limit: FrameLimit, refresh_rate: int, entire_monitor: bool, monitor: nativeptr<Monitor>) =
        assert(is_game_thread())
        uses_compositor <- not entire_monitor
        strategy <-
            match frame_limit with
            | FrameLimit.Custom ->
                GLFW.SwapInterval(0)
                FrameCap (1000.0 / float refresh_rate)

            | FrameLimit.Smart ->
                // On windows: smart cap = do some cool stuff for frame times that outperforms vsync
                // Disabled on Intel to see if it solves the random crashes
                if FrameTimeStrategies.VBlankThread.ENABLED then
                    GLFW.SwapInterval(0)
                    FrameTimeStrategies.VBlankThread.switch (1000.0 / float refresh_rate) (GLFW.GetWin32Adapter monitor) (GLFW.GetWin32Monitor monitor)
                    if entire_monitor then WindowsVblankSync else WindowsDwmFlush
                // On non-windows: smart cap = vsync
                else
                    GLFW.SwapInterval(1)
                    Unlimited

            | _ ->
                GLFW.SwapInterval(0)
                Unlimited

    let private dispatch_frame (ui_root: UIEntryPoint) =

        visual_latency_lo <- real_next_frame - frame_is_ready
        visual_latency_hi <- real_next_frame - start_of_frame

        match strategy with
        | Unlimited -> ignore ()

        | FrameCap frame_time ->
            estimated_next_frame <- start_of_frame + frame_time
            FrameTimeStrategies.sleep_accurate (total_frame_timer, start_of_frame + frame_time)

        | WindowsVblankSync ->
            let last_vblank, est_refresh_period = FrameTimeStrategies.VBlankThread.get(tearline_position, total_frame_timer)
            estimated_next_frame <- last_vblank + est_refresh_period

            let time_taken_to_render = frame_is_ready - start_of_frame
            FrameTimeStrategies.sleep_accurate (total_frame_timer, now() - time_taken_to_render + est_refresh_period / framerate_multiplier)

        | WindowsDwmFlush ->
            FrameTimeStrategies.DwmFlush() |> ignore

            let last_vblank, est_refresh_period = FrameTimeStrategies.VBlankThread.get(tearline_position, total_frame_timer)
            estimated_next_frame <- last_vblank + est_refresh_period

        let elapsed_ms = last_frame_timer.Elapsed.TotalMilliseconds
        last_frame_timer.Restart()

        // Update
        start_of_frame <- now ()
        ACTION_QUEUE.RunQueue()
        Input.begin_frame_events ()
        Audio.update elapsed_ms
        ui_root.Update(elapsed_ms, resized)
        resized <- false
        Input.finish_frame_events ()
        update_time <- now () - start_of_frame

        // Draw
        let before_draw = now ()
        Render.start ()

        if Render._framebuffer_height > 0 then
            ui_root.Draw()

        Render.finish ()
        frame_is_ready <- now ()
        draw_time <- frame_is_ready - before_draw

        GLFW.SwapBuffers(window)
        real_next_frame <- now ()

        // Performance profiling
        fps_count <- fps_count + 1
        let time = fps_timer.ElapsedTicks

        if time > Stopwatch.Frequency then
            framecount_tickcount <- (fps_count, time)
            fps_timer.Restart()
            fps_count <- 0

        update_draw_elapsed_ms <- elapsed_ms

    let private main_loop () =
        GLFW.MakeContextCurrent(window)
        Render.init Render.DEFAULT_SCREEN (Render.DEFAULT_SCREEN, (0.5f, 0.5f))

        match loading_icon with
        | Some icon -> () // todo: get it to work
        | _ -> ()

        match
            try
                let root = ui_initialiser()
                root.Init()
                Ok(root)
            with fatal_err -> Error fatal_err
        with
        | Error fatal_err ->
            fatal_error <- true
            Logging.Critical "Fatal crash in game initialisation: %O" fatal_err
            GLFW.SetWindowShouldClose(window, true)
        | Ok ui_root ->

        FrameTimeStrategies.VBlankThread.start total_frame_timer

        after_init_ev.Trigger()
        fps_timer.Start()

        Input.begin_frame_events ()
        Input.finish_frame_events ()

        try
            while looping && not (GLFW.WindowShouldClose window) do
                dispatch_frame ui_root
        with fatal_err ->
            fatal_error <- true
            Logging.Critical "Fatal crash in game thread: %O" fatal_err
            GLFW.SetWindowShouldClose(window, true)

        FrameTimeStrategies.VBlankThread.stop ()

    let private thread = Thread(main_loop)

    (*
        Initialisation
    *)

    let internal init(_window: nativeptr<Window>, icon: Bitmap option, init_thunk: unit -> UIEntryPoint) =
        ACTION_QUEUE.Bind(thread.ManagedThreadId)
        window <- _window
        ui_initialiser <- init_thunk
        loading_icon <- icon

    let internal start() =
        thread.Start()

    let internal stop() =
        looping <- false
        thread.Join()