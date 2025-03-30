namespace Percyqaz.Flux.Windowing

open System
open System.Threading
open System.Runtime.InteropServices
open FSharp.NativeInterop
open OpenTK.Windowing.Desktop
open OpenTK.Graphics.OpenGL
open OpenTK.Windowing.GraphicsLibraryFramework
open Percyqaz.Common
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics

#nowarn "9"

module WindowThread =

    let mutable private window: nativeptr<Window> = Unchecked.defaultof<_>
    let private LOCK_OBJ = obj()

    (*
        Action queuing

        Most of the game runs from the 'game thread' where draws and updates take place
        `defer` can be used to queue up an action that needs to execute on the window thread

        Deferred actions are fire-and-forget, they will execute in the order they are queued
    *)

    let mutable internal WINDOW_THREAD_ID = -1
    let is_window_thread() = Thread.CurrentThread.ManagedThreadId = WINDOW_THREAD_ID

    let mutable private action_queue : (unit -> unit) list = []
    let private run_action_queue() =
        lock (LOCK_OBJ) (fun () ->
            while not (List.isEmpty action_queue) do
                let actions = action_queue
                action_queue <- []
                (for action in actions do action())
        )
    let defer (action: unit -> unit) =
        lock (LOCK_OBJ) (fun () -> action_queue <- action_queue @ [ action ])
        GLFW.PostEmptyEvent()

    (*
        Monitor detection

        A list of monitors is (re)populated automatically whenever window settings are applied
        It can be fetched from any thread using `get_monitors`
    *)

    type MonitorDetails =
        {
            Id: int
            FriendlyName: string
            DisplayModes: FullscreenVideoMode array
            Info: MonitorInfo
        }

    let mutable private detected_monitors: MonitorDetails list = []

    let private detect_monitors() =
        let monitors =
            Monitors.GetMonitors()
            |> Seq.indexed
            |> Seq.map (fun (i, m) ->
                {
                    Id = i
                    FriendlyName = sprintf "%i: %s" (i + 1) m.Name
                    DisplayModes =
                        GLFW.GetVideoModes(m.Handle.ToUnsafePtr<Monitor>())
                        |> Array.map (fun glfw_mode ->
                            {
                                Width = glfw_mode.Width
                                Height = glfw_mode.Height
                                RefreshRate = glfw_mode.RefreshRate
                            }
                        )
                    Info = m
                }
            )
            |> List.ofSeq
        lock (LOCK_OBJ) (fun () -> detected_monitors <- monitors)

    let get_monitors() = lock (LOCK_OBJ) (fun () -> detected_monitors)

    (*
        Window options

        `apply_config` applies new WindowOptions settings to the game window + render thread
    *)

    let mutable private last_applied_config : WindowOptions = Unchecked.defaultof<_>
    let mutable private refresh_rate = 60
    let mutable private letterbox: WindowedResolution option = None

    let apply_config(config: WindowOptions) =
        assert(is_window_thread())
        detect_monitors()

        letterbox <- if config.WindowMode = WindowType.FullscreenLetterbox then Some config.WindowedResolution else None

        let was_fullscreen = not (NativePtr.isNullPtr (GLFW.GetWindowMonitor(window)))

        let monitor =
            if config.WindowMode <> WindowType.Windowed then
                match List.tryItem config.Display detected_monitors with
                | None -> Monitors.GetMonitorFromWindow(window)
                | Some m -> m.Info
            else
                Monitors.GetMonitorFromWindow(window)
        let monitor_ptr = monitor.Handle.ToUnsafePtr<Monitor>()

        match config.WindowMode with

        | WindowType.Windowed ->
            let width, height = fst config.WindowedResolution
            let offset_x, offset_y = snd config.WindowedResolution
            let x =
                float32 (monitor.ClientArea.Size.X - width) * offset_x
                |> round
                |> int
                |> max monitor.ClientArea.Min.X
                |> min (monitor.ClientArea.Max.X - width)

            let y =
                float32 (monitor.ClientArea.Size.Y - height) * offset_y
                |> round
                |> int
                |> max monitor.ClientArea.Min.Y
                |> min (monitor.ClientArea.Max.Y - height)

            if was_fullscreen then
                GLFW.SetWindowMonitor(
                    window,
                    NativePtr.nullPtr<Monitor>,
                    x, y,
                    width,
                    height,
                    0
                )
                GLFW.SetWindowAttrib(window, WindowAttribute.Decorated, true)
            else
                GLFW.SetWindowAttrib(window, WindowAttribute.Decorated, true)
                GLFW.SetWindowSize(window, width, height)
                GLFW.SetWindowPos(window, x, y)

        | WindowType.Borderless ->
            if was_fullscreen then
                GLFW.SetWindowMonitor(
                    window,
                    NativePtr.nullPtr<Monitor>,
                    monitor.WorkArea.Min.X + 1,
                    monitor.WorkArea.Min.Y + 1,
                    monitor.WorkArea.Size.X - 2,
                    monitor.WorkArea.Size.Y - 2,
                    0
                )
                GLFW.SetWindowAttrib(window, WindowAttribute.Decorated, false)
                GLFW.HideWindow(window)
                GLFW.MaximizeWindow(window)
                GLFW.ShowWindow(window)
            else
                GLFW.SetWindowAttrib(window, WindowAttribute.Decorated, false)
                GLFW.HideWindow(window)
                GLFW.SetWindowPos(window, monitor.WorkArea.Min.X + 1, monitor.WorkArea.Min.Y + 1)
                GLFW.SetWindowSize(window, monitor.WorkArea.Size.X - 2, monitor.WorkArea.Size.Y - 2)
                GLFW.MaximizeWindow(window)
                GLFW.ShowWindow(window)

        | WindowType.BorderlessNoTaskbar ->
            if was_fullscreen then
                GLFW.SetWindowMonitor(
                    window,
                    NativePtr.nullPtr<Monitor>,
                    monitor.ClientArea.Min.X + 1,
                    monitor.ClientArea.Min.Y + 1,
                    monitor.ClientArea.Size.X - 2,
                    monitor.ClientArea.Size.Y - 2,
                    0
                )
            GLFW.HideWindow(window)
            GLFW.SetWindowAttrib(window, WindowAttribute.Decorated, false)
            GLFW.SetWindowPos(window, monitor.ClientArea.Min.X, monitor.ClientArea.Min.Y)
            GLFW.SetWindowSize(window, monitor.ClientArea.Size.X, monitor.ClientArea.Size.Y)
            GLFW.ShowWindow(window)

        | WindowType.Fullscreen ->
            GLFW.SetWindowMonitor(
                window,
                monitor_ptr,
                0,
                0,
                config.FullscreenVideoMode.Width,
                config.FullscreenVideoMode.Height,
                config.FullscreenVideoMode.RefreshRate
            )
            GameThread.defer (fun () ->
                GameThread.framebuffer_resized
                    (config.FullscreenVideoMode.Width, config.FullscreenVideoMode.Height)
                    ((config.FullscreenVideoMode.Width, config.FullscreenVideoMode.Height), (0.5f, 0.5f))
            )

        | WindowType.FullscreenLetterbox ->
            if was_fullscreen && last_applied_config.WindowMode = WindowType.Fullscreen then
                GLFW.IconifyWindow(window)

            let mode = GLFW.GetVideoMode(monitor_ptr) |> NativePtr.read
            let max_width, max_height = mode.Width, mode.Height

            GLFW.SetWindowMonitor(
                window,
                monitor_ptr,
                0,
                0,
                max_width,
                max_height,
                config.FullscreenVideoMode.RefreshRate
            )
            if was_fullscreen then GLFW.RestoreWindow(window)
            GameThread.defer (fun () -> GameThread.framebuffer_resized (max_width, max_height) config.WindowedResolution)

        | _ -> Logging.Error "Tried to change to invalid window mode"

        refresh_rate <-
            if config.RenderMode = FrameLimit.Custom then
                config.CustomFrameLimit
            else
                NativePtr.read(GLFW.GetVideoMode(monitor_ptr)).RefreshRate

        InputThread.set_cursor_hidden config.HideCursor window

        let x, y = GLFW.GetWindowPos(window)
        let width, height = GLFW.GetWindowSize(window)
        let is_entire_monitor =
            config.WindowMode = WindowType.Fullscreen ||
            (
                monitor.ClientArea.Min.X = x && monitor.ClientArea.Max.X = x + width
                && monitor.ClientArea.Min.Y = y && monitor.ClientArea.Max.Y = y + height
            )

        last_applied_config <- config

        if OperatingSystem.IsWindows() then
            Thread.CurrentThread.Priority <-
                if last_applied_config.InputCPUSaver then ThreadPriority.Highest
                else ThreadPriority.Normal

        GameThread.defer
        <| fun () ->
            GameThread.change_mode(config.RenderMode, refresh_rate, is_entire_monitor, monitor_ptr)
            GameThread.anti_jitter <- config.SmartCapAntiJitter
            GameThread.tearline_position <- config.SmartCapTearlinePosition
            GameThread.framerate_multiplier <- config.SmartCapFramerateMultiplier

    (*
        Disabling windows key & Focus

        Hitting the windows key and having it unfocus the game/bring up the start menu can ruin gameplay
        On windows only, calling `disable_windows_key` will disable the windows key while the window is focused
        Calling `enable_windows_key` after gameplay is over will enable the key again
    *)

    let mutable private is_focused = false
    let mutable private disabled_windows_key = false

    let private should_disable_windows_key() =
        disabled_windows_key
        && is_focused
        && not last_applied_config.InputCPUSaver
        && OperatingSystem.IsWindows()

    let disable_windows_key() =
        assert(is_window_thread())
        disabled_windows_key <- true
        if should_disable_windows_key() then
            WindowsKey.disable()

    let enable_windows_key() =
        assert(is_window_thread())
        disabled_windows_key <- false
        WindowsKey.enable()

    let focus_window() =
        assert(is_window_thread())
        GLFW.RestoreWindow(window)
        GLFW.FocusWindow(window)

    let private focus_callback (_: nativeptr<Window>) (focused: bool) =
        is_focused <- focused
        if should_disable_windows_key() then
            WindowsKey.disable()
        else WindowsKey.enable()

    let private focus_callback_d = GLFWCallbacks.WindowFocusCallback focus_callback

    (*
        GLFW initialisation
    *)

    let private framebuffer_size_callback (_: nativeptr<Window>) (buffer_width: int) (buffer_height: int) =
        if buffer_width <> 0 && buffer_height <> 0 then
            match letterbox with
            | Some ((w, h), (x, y)) when w <= buffer_width && h <= buffer_height ->
                GameThread.defer (fun () -> GameThread.framebuffer_resized (buffer_width, buffer_height) ((w, h), (x, y)))
            | _ ->
                GameThread.defer (fun () -> GameThread.framebuffer_resized (buffer_width, buffer_height) ((buffer_width, buffer_height), (0.5f, 0.5f)))

    let private framebuffer_size_callback_d = GLFWCallbacks.FramebufferSizeCallback framebuffer_size_callback

    let private file_drop_ev = Event<string array>()
    let on_file_drop = file_drop_ev.Publish

    let private file_drop_callback (_: nativeptr<Window>) (count: int) (paths: nativeptr<nativeptr<byte>>) =
        let paths_array: string array = Array.zeroCreate count
        try
            for i = 0 to count - 1 do
                paths_array.[i] <- Marshal.PtrToStringUTF8(NativePtr.toNativeInt (NativePtr.get paths i))
        with err ->
            Logging.Critical "Error getting dropped file paths: %O" err
        GameThread.defer (fun () -> file_drop_ev.Trigger paths_array)

    let private file_drop_callback_d = GLFWCallbacks.DropCallback file_drop_callback

    let private error_callback (code: ErrorCode) (desc: string) =
        Logging.Debug "GLFW Error (%O): %s" code desc

    let private error_callback_d = GLFWCallbacks.ErrorCallback error_callback

    let internal init(config: WindowOptions, title: string, init_thunk: unit -> UIEntryPoint, icon: Bitmap option) =
        last_applied_config <- config
        WINDOW_THREAD_ID <- Environment.CurrentManagedThreadId

        GLFWProvider.EnsureInitialized()
        GLFW.SetErrorCallback(error_callback_d) |> ignore

        GLFW.WindowHint(WindowHintBool.Resizable, false)
        GLFW.WindowHint(WindowHintBool.TransparentFramebuffer, true)
        GLFW.WindowHint(WindowHintBool.Visible, false)
        GLFW.WindowHint(WindowHintBool.Focused, false)
        GLFW.WindowHint(WindowHintBool.AutoIconify, true)
        GLFW.WindowHint(WindowHintBool.Decorated, false)
        GLFW.WindowHint(WindowHintOpenGlProfile.OpenGlProfile, OpenGlProfile.Core)
        GLFW.WindowHint(WindowHintBool.OpenGLForwardCompat, true)
        GLFW.WindowHint(WindowHintInt.ContextVersionMajor, 3)
        GLFW.WindowHint(WindowHintInt.ContextVersionMinor, 3)
        GLFW.WindowHint(WindowHintInt.Samples, config.MSAASamples)
        GLFW.WindowHint(WindowHintInt.StencilBits, 8)
        GLFW.WindowHint(WindowHintInt.DepthBits, 8)

        detect_monitors()

        let width, height = fst config.WindowedResolution
        window <- GLFW.CreateWindow(width, height, title, Unchecked.defaultof<_>, Unchecked.defaultof<_>)

        if NativePtr.isNullPtr window then
            let mutable desc = ""
            let error = GLFW.GetError(&desc)
            match error with
            | ErrorCode.VersionUnavailable ->
                Logging.Critical ""
                Logging.Critical "=== Helpful message from Percyqaz ==="
                Logging.Critical "This error means your PC doesn't support OpenGL 3.3 or higher, and so can't run '%s' :(" title
                Logging.Critical "If you think your PC DOES support OpenGL 3.3+, make sure you have up-to-date drivers; Complain in the Discord if the problem persists"
                Logging.Critical "If your PC is from about 2011/12 and supports OpenGL 3.0-3.2, complain in the Discord anyway and I could spend more time seeing if the engine can be more backwards compatible"
                Logging.Critical ""
            | ErrorCode.ApiUnavailable ->
                Logging.Critical ""
                Logging.Critical "=== Helpful message from Percyqaz ==="
                Logging.Critical "This error says your PC doesn't support OpenGL"
                Logging.Critical "This is probably not actually true, make sure you have up-to-date drivers; Complain in the Discord if the problem persists"
                Logging.Critical ""
            | _ -> ()
            failwithf "GLFW failed to create window: %A\n%s" error desc

        let context = GLFWGraphicsContext(window)
        let bindings = GLFWBindingsContext()
        context.MakeCurrent()
        GL.LoadBindings(bindings)
        context.MakeNoneCurrent()

        match icon with
        | Some icon ->
            let mutable pixel_data = System.Span<SixLabors.ImageSharp.PixelFormats.Rgba32>.Empty
            let success = icon.TryGetSinglePixelSpan(&pixel_data)
            if not success then failwithf "Couldn't get pixel span for icon"
            use pixel_data_ref = fixed pixel_data
            let image_array = [| new Image(icon.Width, icon.Height, pixel_data_ref |> NativePtr.toNativeInt |> NativePtr.ofNativeInt) |]
            GLFW.SetWindowIcon(window, System.Span<Image>(image_array))
        | None -> ()

        GameThread.init(window, icon, init_thunk)
        Audio.init(config.AudioDevice, config.AudioDevicePeriod, config.AudioDevicePeriod * config.AudioDeviceBufferLengthMultiplier)
        letterbox <- if config.WindowMode = WindowType.FullscreenLetterbox then Some config.WindowedResolution else None
        framebuffer_size_callback window width height

        GLFW.SetInputMode(window, RawMouseMotionAttribute.RawMouseMotion, true)
        Input.init window

        GLFW.SetDropCallback(window, file_drop_callback_d) |> ignore
        GLFW.SetFramebufferSizeCallback(window, framebuffer_size_callback_d) |> ignore
        GLFW.SetWindowFocusCallback(window, focus_callback_d) |> ignore

        let monitor_area = Monitors.GetMonitorFromWindow(window).ClientArea
        GLFW.SetWindowPos(window, (monitor_area.Min.X + monitor_area.Max.X - width) / 2, (monitor_area.Min.Y + monitor_area.Max.Y - height) / 2)
        GLFW.ShowWindow(window)
        GLFW.RestoreWindow(window)
        GLFW.FocusWindow(window)

    let internal run() =
        Fonts.init()
        GameThread.defer (fun () -> defer (fun () -> apply_config last_applied_config))
        GameThread.start()

        while not (GLFW.WindowShouldClose window) do
            run_action_queue()
            if last_applied_config.InputCPUSaver then
                GLFW.WaitEventsTimeout(5)
            else
                GLFW.PollEvents()

        WindowsKey.enable()
        GameThread.stop()
        GLFW.MakeContextCurrent(NativePtr.nullPtr<Window>)
        GLFW.DestroyWindow(window)
        GLFW.Terminate()

        if GameThread.has_fatal_error() then Error() else Ok()

    let exit () =
        GLFW.SetWindowShouldClose(window, true)
        GLFW.PostEmptyEvent()

    let debug_info() : string =
        assert(not (NativePtr.isNullPtr window))
        assert(is_window_thread())

        let monitor = Monitors.GetMonitorFromWindow(window)
        let default_monitor = Monitors.GetPrimaryMonitor()

        let monitor_dump (info: MonitorInfo) =
            let max_hz_vm = info.SupportedVideoModes |> Seq.maxBy (fun vm -> (vm.RefreshRate, vm.Width, vm.Height))
            let max_res_vm = info.SupportedVideoModes |> Seq.maxBy (fun vm -> (vm.Width, vm.Height, vm.RefreshRate))

            sprintf "%s (%i, %i) %ix%i | S: %imm x %imm | DPI: %.4f x %.4f\n\tVideo mode: %ix%i@%ihz [%i:%i:%i]\n\tMax Refresh Rate: %ix%i@%ihz\n\tMax Resolution: %ix%i@%ihz"
                info.Name
                info.ClientArea.Min.X
                info.ClientArea.Min.Y
                info.ClientArea.Size.X
                info.ClientArea.Size.Y
                info.PhysicalHeight
                info.PhysicalWidth
                info.HorizontalDpi
                info.VerticalDpi
                info.CurrentVideoMode.Width
                info.CurrentVideoMode.Height
                info.CurrentVideoMode.RefreshRate
                info.CurrentVideoMode.RedBits
                info.CurrentVideoMode.GreenBits
                info.CurrentVideoMode.BlueBits
                max_hz_vm.Width
                max_hz_vm.Height
                max_hz_vm.RefreshRate
                max_res_vm.Width
                max_res_vm.Height
                max_res_vm.RefreshRate

        sprintf
            """-- WINDOW DEBUG INFO --
Display: %i
Current Monitor: %s
Primary Monitor: %s
Mode: %O %O | W: %A TL: %f AJ: %A FM: %.0fx
CPU Saver: %A"""
            last_applied_config.Display
            (monitor_dump monitor)
            (monitor_dump default_monitor)
            last_applied_config.RenderMode
            last_applied_config.WindowMode
            last_applied_config.WindowedResolution
            last_applied_config.SmartCapTearlinePosition
            last_applied_config.SmartCapAntiJitter
            last_applied_config.SmartCapFramerateMultiplier
            last_applied_config.InputCPUSaver