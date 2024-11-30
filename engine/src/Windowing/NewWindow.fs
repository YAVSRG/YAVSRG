namespace Percyqaz.Flux.Windowing

open System
open System.Threading
open FSharp.NativeInterop
open OpenTK.Windowing.Desktop
open OpenTK.Windowing.Common
open OpenTK.Windowing.GraphicsLibraryFramework
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics

#nowarn "9"

module NewWindow =

    open System.Runtime.InteropServices

    type MonitorDetails =
        {
            Id: int
            FriendlyName: string
            DisplayModes: FullscreenVideoMode array
        }

    let mutable private window: nativeptr<Window> = Unchecked.defaultof<_>
    let mutable private render_thread: RenderThread = Unchecked.defaultof<_>

    let mutable private detected_monitors: MonitorDetails list = []
    let private LOCK_OBJ = obj()

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
                }
            )
            |> List.ofSeq
        lock (LOCK_OBJ) (fun () -> detected_monitors <- monitors)

    let get_monitors() = lock (LOCK_OBJ) (fun () -> detected_monitors)

    let private error_callback (code: ErrorCode) (desc: string) =
        Logging.Debug(sprintf "GLFW Error (%O): %s" code desc)

    let private error_callback_d = GLFWCallbacks.ErrorCallback error_callback

    let private file_drop_ev = Event<string array>()
    let on_file_drop = file_drop_ev.Publish

    let private file_drop_callback (_: nativeptr<Window>) (count: int) (paths: nativeptr<nativeptr<byte>>) =
        let paths_array: string array = Array.zeroCreate count
        try
            for i = 0 to count - 1 do
                paths_array.[i] <- Marshal.PtrToStringUTF8(NativePtr.toNativeInt (NativePtr.get paths i))
        with err ->
            Logging.Critical("Error getting dropped file paths", err)
        file_drop_ev.Trigger paths_array

    let private file_drop_callback_d = GLFWCallbacks.DropCallback file_drop_callback

    let resize_callback (window: nativeptr<Window>) (_: int) (_: int) =
        let width, height = GLFW.GetFramebufferSize(window)
        defer (fun () ->
            if width <> 0 && height <> 0 then
                render_thread.OnResize(width, height)
        )
        
    let private resize_callback_d = GLFWCallbacks.WindowSizeCallback resize_callback

    let init(config: WindowingUserOptionsSnapshot, title: string, ui_root: Root) =
        GLFWProvider.EnsureInitialized()
        GLFW.WindowHint(WindowHintBool.Resizable, false)
        GLFW.WindowHint(WindowHintBool.Focused, true)
        GLFW.WindowHint(WindowHintBool.AutoIconify, true)
        GLFW.WindowHint(WindowHintBool.Decorated, false)
        GLFW.WindowHint(WindowHintBool.OpenGLForwardCompat, true)
        GLFW.WindowHint(WindowHintInt.ContextVersionMajor, 3)
        GLFW.WindowHint(WindowHintInt.ContextVersionMinor, 3)
        GLFW.WindowHint(WindowHintInt.Samples, (if OperatingSystem.IsMacOS() then 0 else 24))
        GLFW.WindowHint(WindowHintInt.StencilBits, 8)
        GLFW.WindowHint(WindowHintInt.DepthBits, 8)
        GLFW.WindowHint(WindowHintBool.TransparentFramebuffer, true)

        detect_monitors()

        window <- GLFW.CreateWindow(400, 400, title, Monitors.GetPrimaryMonitor().Handle.ToUnsafePtr(), Unchecked.defaultof<_>)

        if NativePtr.isNullPtr window then
            let mutable desc = ""
            let error = GLFW.GetError(&desc)
            failwithf "GLFW failed to create window: %A\n%s" error desc

        let context = GLFWGraphicsContext(window)
        let bindings = GLFWBindingsContext()
        context.MakeCurrent()
        OpenTK.Graphics.OpenGL.GL.LoadBindings(bindings)
        // might need GL4 too?

        context.MakeNoneCurrent()

        render_thread <- RenderThread(
            window,
            context,
            config.AudioDevice,
            config.AudioDevicePeriod,
            config.AudioDevicePeriod * config.AudioDeviceBufferLengthMultiplier,
            ui_root,
            (fun () -> printfn "after init")
        )

        Input.init window

        GLFW.SetDropCallback(window, file_drop_callback_d) |> ignore
        GLFW.SetWindowSizeCallback(window, resize_callback_d) |> ignore

    let run() =
        // on load
        Fonts.init()
        render_thread.Start()

        //if input_cpu_saver && OperatingSystem.IsWindows() then
        //    Thread.CurrentThread.Priority <- ThreadPriority.Highest

        while not (GLFW.WindowShouldClose window) do
            //lock
            //    Window.LOCK_OBJ
            //    (fun () ->
            //        for a in Window.action_queue do
            //            match a with
            //            | Window.ApplyConfig c -> this.ApplyConfig c
            //            | Window.EnableResize c -> this.EnableResize c
            //            | Window.DisableResize -> this.DisableResize()
            //            | Window.DisableWindowsKey ->
            //                disable_windows_key <- true
            //                if should_disable_windows_key() then
            //                    WindowsKey.disable()
            //            | Window.EnableWindowsKey ->
            //                disable_windows_key <- false
            //                WindowsKey.enable()

            //        Window.action_queue <- []
            //    )

            //if input_cpu_saver then
            //    GLFW.WaitEventsTimeout(0.5)
            //else
                GLFW.PollEvents()

        // on unload