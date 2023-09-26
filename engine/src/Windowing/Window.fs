namespace Percyqaz.Flux.Windowing

open System.Threading
open OpenTK
open OpenTK.Mathematics
open OpenTK.Windowing.Desktop
open OpenTK.Windowing.Common
open OpenTK.Windowing.GraphicsLibraryFramework
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics

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
    inherit NativeWindow(NativeWindowSettings(StartVisible = false, NumberOfSamples = 24))

    let renderThread = RenderThread(this, config.AudioDevice.Value, root, WindowEvents.afterInit.Trigger)

    let mutable resize_callback = fun (w, h) -> ()
    let mutable refresh_rate = 60

    do
        base.Title <- title
        base.VSync <- VSyncMode.Off
        base.CursorState <- CursorState.Hidden

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

        refresh_rate <- monitor.CurrentVideoMode.RefreshRate
        renderThread.RenderMode <- config.RenderMode.Value

        let was_fullscreen = base.WindowState = WindowState.Fullscreen

        match config.WindowMode.Value with

        | WindowType.Windowed ->
            base.WindowState <- WindowState.Normal
            if was_fullscreen then Thread.Sleep(100)
            let width, height = config.WindowResolution.Value
            base.WindowBorder <- WindowBorder.Fixed
            base.ClientRectangle <- new Box2i(monitor.ClientArea.Min.X, monitor.ClientArea.Min.Y, monitor.ClientArea.Min.X + width, monitor.ClientArea.Min.Y + height)
            base.CenterWindow()

        | WindowType.Borderless ->
            base.WindowState <- WindowState.Normal
            if was_fullscreen then Thread.Sleep(100)
            base.ClientRectangle <- new Box2i(monitor.ClientArea.Min - Vector2i(1, 1), monitor.ClientArea.Max + Vector2i(1, 1))
            base.WindowBorder <- WindowBorder.Hidden
            base.WindowState <- WindowState.Maximized

        | WindowType.Fullscreen ->
            base.WindowState <- WindowState.Fullscreen
            let monitor = monitor.Handle.ToUnsafePtr<Monitor>()
            let modes = GLFW.GetVideoModes(monitor)
            let mode = modes.[modes.Length - 1]
            GLFW.SetWindowMonitor(this.WindowPtr, monitor, 0, 0, mode.Width, mode.Height, max config.FullscreenRefreshRateOverride.Value mode.RefreshRate)

        | WindowType.``Borderless Fullscreen`` ->
            base.WindowState <- WindowState.Normal
            if was_fullscreen then Thread.Sleep(100)
            base.WindowBorder <- WindowBorder.Hidden
            base.ClientRectangle <- new Box2i(monitor.ClientArea.Min, monitor.ClientArea.Max)
            base.CenterWindow()

        | _ -> Logging.Error "Tried to change to invalid window mode"

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