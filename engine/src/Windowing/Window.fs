namespace Percyqaz.Flux.Windowing

open System
open FSharp.NativeInterop
open OpenTK
open OpenTK.Windowing.Desktop
open OpenTK.Windowing.Common
open OpenTK.Windowing.GraphicsLibraryFramework
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics

#nowarn "9"

module private WindowEvents =

    let onLoad = Event<unit>()
    let afterInit = Event<unit>()
    let onUnload = Event<unit>()
    let onFileDrop = Event<string>()
    let onResize = Event<unit>()

module Window =
    
    let onLoad = WindowEvents.onLoad.Publish
    let afterInit = WindowEvents.afterInit.Publish
    let onUnload = WindowEvents.onUnload.Publish
    let onFileDrop = WindowEvents.onFileDrop.Publish
    let onResize = WindowEvents.onResize.Publish

    let lockObj = obj()
    let mutable action_queue = []

    type WindowAction =
        | ApplyConfig of Config
        | EnableResize of callback: ((int * int) -> unit)
        | DisableResize

    let sync (a: WindowAction) = lock (lockObj) (fun () -> action_queue <- action_queue @ [a])
    let mutable monitors = [||]


[<Sealed>]
type Window(config: Config, title: string, root: Root) as this =
    inherit NativeWindow(NativeWindowSettings(StartVisible = false, NumberOfSamples = (if OperatingSystem.IsMacOS() then 0 else 24), Flags = ContextFlags.ForwardCompatible, Profile = ContextProfile.Core))

    let renderThread = RenderThread(this, config.AudioDevice.Value, root, WindowEvents.afterInit.Trigger)

    let mutable resize_callback = fun (w, h) -> ()
    let mutable refresh_rate = 60
    let mutable was_fullscreen = false

    do
        base.Title <- title
        base.VSync <- VSyncMode.Off
        base.CursorState <- if OperatingSystem.IsMacOS() then CursorState.Grabbed else CursorState.Hidden

    member this.ApplyConfig(config: Config) =

        let monitor_list = Monitors.GetMonitors()

        Window.monitors <-
            monitor_list
            |> Seq.indexed
            |> Seq.map (fun (i, m) -> i, sprintf "%i: %s" (i + 1) m.Name)
            |> Array.ofSeq

        let monitor =
            if config.WindowMode.Value <> WindowType.Windowed then
                try
                    monitor_list.[config.Display.Value]
                with err ->
                    Logging.Error (sprintf "Failed to get display info for monitor %i" config.Display.Value)
                    Monitors.GetMonitorFromWindow(this)
            else Monitors.GetMonitorFromWindow(this)

        renderThread.RenderMode <- config.RenderMode.Value

        let monitorPtr = monitor.Handle.ToUnsafePtr<Monitor>()

        if was_fullscreen then

            match config.WindowMode.Value with

            | WindowType.Windowed ->
                let width, height = config.WindowResolution.Value
                GLFW.SetWindowMonitor(this.WindowPtr, NativePtr.nullPtr<Monitor>, monitor.ClientArea.Min.X, monitor.ClientArea.Min.Y, width, height, 0)
                base.CenterWindow()
                base.WindowBorder <- WindowBorder.Fixed
                refresh_rate <- NativePtr.read(GLFW.GetVideoMode(monitorPtr)).RefreshRate

            | WindowType.Borderless ->
                GLFW.SetWindowMonitor(this.WindowPtr, NativePtr.nullPtr<Monitor>, monitor.ClientArea.Min.X - 1, monitor.ClientArea.Min.Y - 1, monitor.ClientArea.Size.X + 1, monitor.ClientArea.Size.Y + 1, 0)
                base.WindowBorder <- WindowBorder.Hidden
                GLFW.HideWindow(this.WindowPtr)
                GLFW.MaximizeWindow(this.WindowPtr)
                GLFW.ShowWindow(this.WindowPtr)
                refresh_rate <- NativePtr.read(GLFW.GetVideoMode(monitorPtr)).RefreshRate

            | WindowType.``Borderless Fullscreen`` ->
                GLFW.SetWindowMonitor(this.WindowPtr, NativePtr.nullPtr<Monitor>, monitor.ClientArea.Min.X, monitor.ClientArea.Min.Y, monitor.ClientArea.Size.X, monitor.ClientArea.Size.Y, 0)
                base.WindowBorder <- WindowBorder.Hidden
                base.CenterWindow()
                refresh_rate <- NativePtr.read(GLFW.GetVideoMode(monitorPtr)).RefreshRate

            | WindowType.Fullscreen ->
                let modes = GLFW.GetVideoModes(monitorPtr)
                let mode = modes.[modes.Length - 1]
                refresh_rate <- max config.FullscreenRefreshRateOverride.Value mode.RefreshRate
                GLFW.SetWindowMonitor(this.WindowPtr, monitorPtr, 0, 0, mode.Width, mode.Height, refresh_rate)

            | _ -> Logging.Error "Tried to change to invalid window mode"

        else
            
            match config.WindowMode.Value with

            | WindowType.Windowed ->
                base.WindowBorder <- WindowBorder.Fixed
                let width, height = config.WindowResolution.Value
                GLFW.SetWindowSize(this.WindowPtr, width, height)
                base.CenterWindow()
                refresh_rate <- NativePtr.read(GLFW.GetVideoMode(monitorPtr)).RefreshRate

            | WindowType.Borderless ->
                base.WindowBorder <- WindowBorder.Hidden
                GLFW.HideWindow(this.WindowPtr)
                GLFW.SetWindowPos(this.WindowPtr, monitor.ClientArea.Min.X - 1, monitor.ClientArea.Min.Y - 1)
                GLFW.SetWindowSize(this.WindowPtr, monitor.ClientArea.Size.X + 1, monitor.ClientArea.Size.Y + 1)
                GLFW.MaximizeWindow(this.WindowPtr)
                GLFW.ShowWindow(this.WindowPtr)
                refresh_rate <- NativePtr.read(GLFW.GetVideoMode(monitorPtr)).RefreshRate

            | WindowType.``Borderless Fullscreen`` ->
                base.WindowBorder <- WindowBorder.Hidden
                GLFW.SetWindowPos(this.WindowPtr, monitor.ClientArea.Min.X, monitor.ClientArea.Min.Y)
                GLFW.SetWindowSize(this.WindowPtr, monitor.ClientArea.Size.X, monitor.ClientArea.Size.Y)
                base.CenterWindow()
                refresh_rate <- NativePtr.read(GLFW.GetVideoMode(monitorPtr)).RefreshRate

            | WindowType.Fullscreen ->
                let modes = GLFW.GetVideoModes(monitorPtr)
                let mode = modes.[modes.Length - 1]
                refresh_rate <- max config.FullscreenRefreshRateOverride.Value mode.RefreshRate
                GLFW.SetWindowMonitor(this.WindowPtr, monitorPtr, 0, 0, mode.Width, mode.Height, refresh_rate)

            | _ -> Logging.Error "Tried to change to invalid window mode"

        was_fullscreen <- config.WindowMode.Value = WindowType.Fullscreen

    member this.EnableResize(callback) =
        if base.WindowState = WindowState.Normal && base.WindowBorder = WindowBorder.Fixed then
            base.WindowBorder <- WindowBorder.Resizable
        resize_callback <- callback

    member this.DisableResize() =
        if base.WindowState = WindowState.Normal && base.WindowBorder = WindowBorder.Resizable then
            base.WindowBorder <- WindowBorder.Fixed

    override this.OnResize e =
        base.OnResize e
        if e.Height <> 0 then
            sync ( fun () -> 
                if this.WindowBorder = WindowBorder.Resizable then resize_callback(this.ClientSize.X, this.ClientSize.Y)
                renderThread.OnResize(this.ClientSize, refresh_rate)
            )

    override this.OnFileDrop e =
        Array.iter WindowEvents.onFileDrop.Trigger e.FileNames

    member this.Run() =
        this.OnLoad()
        this.OnResize(ResizeEventArgs(this.Size))
        
        this.Context.MakeNoneCurrent()
        renderThread.Start()

        while not (GLFW.WindowShouldClose this.WindowPtr) do
            lock (Window.lockObj) (fun () ->
                for a in Window.action_queue do
                    match a with
                    | Window.ApplyConfig c -> this.ApplyConfig c
                    | Window.EnableResize c -> this.EnableResize c
                    | Window.DisableResize -> this.DisableResize()
                Window.action_queue <- []
            )
            this.ProcessInputEvents()
            GLFW.PollEvents()
            InputThread.poll(this.KeyboardState, this.MouseState)

        this.OnUnload()
    
    member this.OnLoad() =
        this.ApplyConfig config
        Fonts.init()
        Input.init this
        Hotkeys.init()
        WindowEvents.onLoad.Trigger()
        base.IsVisible <- true

    member this.OnUnload() =
        WindowEvents.onUnload.Trigger()
