namespace Percyqaz.Flux.Windowing

open System
open System.Threading
open System.Runtime.InteropServices
open FSharp.NativeInterop
open OpenTK.Windowing.Desktop
open OpenTK.Graphics.OpenGL
open Percyqaz.Flux.UI
open OpenTK.Windowing.GraphicsLibraryFramework
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics

#nowarn "9"

module WindowThread =

    let mutable private window: nativeptr<Window> = Unchecked.defaultof<_>
    let mutable private render_thread: RenderThread = Unchecked.defaultof<_>
    let private LOCK_OBJ = obj()

    (*
        Action queuing
        
        Most of the game runs from the 'render thread' where draws and updates take place
        `defer` can be used to queue up an action that needs to execute on the window thread

        Deferred actions are fire-and-forget, they will execute in the order they are queued
    *)
    
    let mutable internal WINDOW_THREAD_ID = -1
    let is_window_thread() =
        Thread.CurrentThread.ManagedThreadId = WINDOW_THREAD_ID

    let mutable private action_queue : (unit -> unit) list = []
    let private run_action_queue() =
        lock (LOCK_OBJ) (fun () -> (for action in action_queue do action()); action_queue <- [])
    let w_defer (action: unit -> unit) =
        lock (LOCK_OBJ) (fun () -> action_queue <- action_queue @ [ action ])

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
        Window config
        
        todo: move the snapshot object here and remove the original
    *)

    let mutable private last_applied_config : WindowOptions = Unchecked.defaultof<_>
    let mutable private refresh_rate = 60

    let apply_config(config: WindowOptions) =
        assert(is_window_thread())
        detect_monitors()

        last_applied_config <- config
        render_thread.RenderMode <- config.RenderMode

        let was_fullscreen = not (NativePtr.isNullPtr (GLFW.GetWindowMonitor(window)))

        let monitor =
            if config.WindowMode <> WindowType.Windowed then
                match List.tryItem config.Display detected_monitors with
                | None -> Monitors.GetMonitorFromWindow(window)
                | Some m -> m.Info
            else
                Monitors.GetMonitorFromWindow(window)
        let monitor_ptr = monitor.Handle.ToUnsafePtr<Monitor>()

        if was_fullscreen then

            match config.WindowMode with

            | WindowType.Windowed ->
                let width, height = config.WindowResolution

                GLFW.SetWindowMonitor(
                    window,
                    NativePtr.nullPtr<Monitor>,
                    (monitor.ClientArea.Min.X + monitor.ClientArea.Max.X - width) / 2,
                    (monitor.ClientArea.Min.Y + monitor.ClientArea.Max.Y - height) / 2,
                    width,
                    height,
                    0
                )
                GLFW.SetWindowAttrib(window, WindowAttribute.Decorated, true)
                refresh_rate <- NativePtr.read(GLFW.GetVideoMode(monitor_ptr)).RefreshRate

            | WindowType.Borderless ->
                GLFW.SetWindowMonitor(
                    window,
                    NativePtr.nullPtr<Monitor>,
                    monitor.ClientArea.Min.X - 1,
                    monitor.ClientArea.Min.Y - 1,
                    monitor.ClientArea.Size.X + 1,
                    monitor.ClientArea.Size.Y + 1,
                    0
                )
                
                GLFW.SetWindowAttrib(window, WindowAttribute.Decorated, false)
                GLFW.HideWindow(window)
                GLFW.MaximizeWindow(window)
                GLFW.ShowWindow(window)
                refresh_rate <- NativePtr.read(GLFW.GetVideoMode(monitor_ptr)).RefreshRate

            | WindowType.BorderlessNoTaskbar ->
                GLFW.SetWindowMonitor(
                    window,
                    NativePtr.nullPtr<Monitor>,
                    monitor.ClientArea.Min.X,
                    monitor.ClientArea.Min.Y,
                    monitor.ClientArea.Size.X,
                    monitor.ClientArea.Size.Y,
                    0
                )
                
                GLFW.SetWindowAttrib(window, WindowAttribute.Decorated, false)
                refresh_rate <- NativePtr.read(GLFW.GetVideoMode(monitor_ptr)).RefreshRate

            | WindowType.Fullscreen ->
                let requested_mode = config.FullscreenVideoMode

                GLFW.SetWindowMonitor(
                    window,
                    monitor_ptr,
                    0,
                    0,
                    requested_mode.Width,
                    requested_mode.Height,
                    requested_mode.RefreshRate
                )

                refresh_rate <- NativePtr.read(GLFW.GetVideoMode(monitor_ptr)).RefreshRate

            | _ -> Logging.Error "Tried to change to invalid window mode"

        else

            match config.WindowMode with

            | WindowType.Windowed ->
                GLFW.SetWindowAttrib(window, WindowAttribute.Decorated, true)
                let width, height = config.WindowResolution
                GLFW.SetWindowSize(window, width, height)
                GLFW.SetWindowPos(window, (monitor.ClientArea.Min.X + monitor.ClientArea.Max.X - width) / 2, (monitor.ClientArea.Min.Y + monitor.ClientArea.Max.Y - height) / 2)
                refresh_rate <- NativePtr.read(GLFW.GetVideoMode(monitor_ptr)).RefreshRate

            | WindowType.Borderless ->
                GLFW.SetWindowAttrib(window, WindowAttribute.Decorated, false)
                GLFW.HideWindow(window)
                GLFW.SetWindowPos(window, monitor.ClientArea.Min.X - 1, monitor.ClientArea.Min.Y - 1)
                GLFW.SetWindowSize(window, monitor.ClientArea.Size.X + 1, monitor.ClientArea.Size.Y + 1)
                GLFW.MaximizeWindow(window)
                GLFW.ShowWindow(window)
                refresh_rate <- NativePtr.read(GLFW.GetVideoMode(monitor_ptr)).RefreshRate

            | WindowType.BorderlessNoTaskbar ->
                GLFW.SetWindowAttrib(window, WindowAttribute.Decorated, false)
                GLFW.SetWindowPos(window, monitor.ClientArea.Min.X, monitor.ClientArea.Min.Y)
                GLFW.SetWindowSize(window, monitor.ClientArea.Size.X, monitor.ClientArea.Size.Y)
                refresh_rate <- NativePtr.read(GLFW.GetVideoMode(monitor_ptr)).RefreshRate

            | WindowType.Fullscreen ->
                let requested_mode = config.FullscreenVideoMode

                GLFW.SetWindowMonitor(
                    window,
                    monitor_ptr,
                    0,
                    0,
                    requested_mode.Width,
                    requested_mode.Height,
                    requested_mode.RefreshRate
                )

                refresh_rate <- NativePtr.read(GLFW.GetVideoMode(monitor_ptr)).RefreshRate

            | _ -> Logging.Error "Tried to change to invalid window mode"
        
        if config.EnableCursor then 
            GLFW.SetInputMode(window, CursorStateAttribute.Cursor, CursorModeValue.CursorNormal)
        else
            GLFW.SetInputMode(window, CursorStateAttribute.Cursor, CursorModeValue.CursorHidden)

        if OperatingSystem.IsWindows() then
            FrameTimeStrategies.VBlankThread.switch (1000.0 / float refresh_rate) (GLFW.GetWin32Adapter monitor_ptr) (GLFW.GetWin32Monitor monitor_ptr)

        let x, y = GLFW.GetWindowPos(window)
        let width, height = GLFW.GetWindowSize(window)

        defer
        <| fun () ->
            render_thread.RenderModeChanged(
                config.WindowMode = WindowType.Fullscreen
                || 
                (
                    monitor.ClientArea.Min.X = x && monitor.ClientArea.Max.X = x + width 
                    && monitor.ClientArea.Min.Y = y && monitor.ClientArea.Max.Y = y + height
                )
            )

            anti_jitter <- config.SmartCapAntiJitter
            tearline_position <- config.SmartCapTearlinePosition
            framerate_multiplier <- config.SmartCapFramerateMultiplier

    (*
        Disabling windows key
        
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

    (* 
        GLFW windowing logic
    *)

    let private focus_callback (_: nativeptr<Window>) (focused: bool) =
        is_focused <- focused
        if should_disable_windows_key() then 
            WindowsKey.disable()
        else WindowsKey.enable()
        
    let private focus_callback_d = GLFWCallbacks.WindowFocusCallback focus_callback

    let private resize_callback (window: nativeptr<Window>) (_: int) (_: int) =
        let width, height = GLFW.GetFramebufferSize(window)
        defer (fun () ->
            if width <> 0 && height <> 0 then
                render_thread.OnResize(width, height)
        )
        
    let private resize_callback_d = GLFWCallbacks.WindowSizeCallback resize_callback

    let private file_drop_ev = Event<string array>()
    let on_file_drop = file_drop_ev.Publish

    let private file_drop_callback (_: nativeptr<Window>) (count: int) (paths: nativeptr<nativeptr<byte>>) =
        let paths_array: string array = Array.zeroCreate count
        try
            for i = 0 to count - 1 do
                paths_array.[i] <- Marshal.PtrToStringUTF8(NativePtr.toNativeInt (NativePtr.get paths i))
        with err ->
            Logging.Critical("Error getting dropped file paths", err)
        defer (fun () -> file_drop_ev.Trigger paths_array)

    let private file_drop_callback_d = GLFWCallbacks.DropCallback file_drop_callback

    let private error_callback (code: ErrorCode) (desc: string) =
        Logging.Debug(sprintf "GLFW Error (%O): %s" code desc)

    let private error_callback_d = GLFWCallbacks.ErrorCallback error_callback

    // todo: extract out ui_root
    let internal init(config: WindowOptions, title: string, ui_root: UIEntryPoint, icon: Percyqaz.Flux.Utils.Bitmap option) =
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
        GLFW.WindowHint(WindowHintBool.OpenGLForwardCompat, true)
        GLFW.WindowHint(WindowHintInt.ContextVersionMajor, 3)
        GLFW.WindowHint(WindowHintInt.ContextVersionMinor, 3)
        GLFW.WindowHint(WindowHintInt.Samples, (if OperatingSystem.IsMacOS() then 0 else 24))
        GLFW.WindowHint(WindowHintInt.StencilBits, 8)
        GLFW.WindowHint(WindowHintInt.DepthBits, 8)

        detect_monitors()

        let INITIAL_SIZE = 400
        
        window <- GLFW.CreateWindow(INITIAL_SIZE, INITIAL_SIZE, title, Unchecked.defaultof<_>, Unchecked.defaultof<_>)

        if NativePtr.isNullPtr window then
            let mutable desc = ""
            let error = GLFW.GetError(&desc)
            failwithf "GLFW failed to create window: %A\n%s" error desc

        let context = GLFWGraphicsContext(window)
        let bindings = GLFWBindingsContext()
        context.MakeCurrent()
        GL.LoadBindings(bindings)
        context.MakeNoneCurrent()

        render_thread <- RenderThread(
            window,
            context,
            config.AudioDevice,
            config.AudioDevicePeriod,
            config.AudioDevicePeriod * config.AudioDeviceBufferLengthMultiplier,
            ui_root
        )

        resize_callback window INITIAL_SIZE INITIAL_SIZE

        Input.init window

        match icon with
        | Some icon ->
            let mutable pixel_data = System.Span<SixLabors.ImageSharp.PixelFormats.Rgba32>.Empty
            let success = icon.TryGetSinglePixelSpan(&pixel_data)
            if not success then failwithf "Couldn't get pixel span for icon"
            use pixel_data_ref = fixed &(pixel_data.GetPinnableReference())
            let image_array = [| new Image(icon.Width, icon.Height, pixel_data_ref |> NativePtr.toNativeInt |> NativePtr.ofNativeInt) |]
            GLFW.SetWindowIcon(window, System.Span<Image>(image_array))
        | None -> ()

        GLFW.SetDropCallback(window, file_drop_callback_d) |> ignore
        GLFW.SetWindowSizeCallback(window, resize_callback_d) |> ignore
        GLFW.SetWindowFocusCallback(window, focus_callback_d) |> ignore

        let monitor_area = Monitors.GetMonitorFromWindow(window).ClientArea
        GLFW.SetWindowPos(window, (monitor_area.Min.X + monitor_area.Max.X - INITIAL_SIZE) / 2, (monitor_area.Min.Y + monitor_area.Max.Y - INITIAL_SIZE) / 2)
        GLFW.ShowWindow(window)
        GLFW.FocusWindow(window)

    let internal run() =
        apply_config last_applied_config
        Fonts.init()
        render_thread.Start()

        if last_applied_config.InputCPUSaver && OperatingSystem.IsWindows() then
            Thread.CurrentThread.Priority <- ThreadPriority.Highest

        while not (GLFW.WindowShouldClose window) do
            run_action_queue()
            if last_applied_config.InputCPUSaver then
                GLFW.WaitEventsTimeout(0.5)
            else
                GLFW.PollEvents()
                
        WindowsKey.enable()
        GLFW.MakeContextCurrent(NativePtr.nullPtr<Window>)
        GLFW.Terminate()

        if render_thread.HasFatalError then Error() else Ok()