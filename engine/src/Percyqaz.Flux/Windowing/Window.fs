namespace Percyqaz.Flux.Windowing

open System
open System.Threading
open OpenTK
open OpenTK.Mathematics
open OpenTK.Windowing.Desktop
open OpenTK.Windowing.Common
open OpenTK.Windowing.GraphicsLibraryFramework
open Percyqaz.Common
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI

module private WindowEvents =

    let onLoad = Event<unit>()
    let onUnload = Event<unit>()
    let onFileDrop = Event<string>()
    let onResize = Event<unit>()

module Window =
    
    let onLoad = WindowEvents.onLoad.Publish
    let onUnload = WindowEvents.onUnload.Publish
    let onFileDrop = WindowEvents.onFileDrop.Publish
    let onResize = WindowEvents.onResize.Publish

    let mutable apply_config = ignore

[<Sealed>]
type Window(config: Config, title: string, root: Root) as this =
    inherit NativeWindow(NativeWindowSettings(StartVisible = false, NumberOfSamples = 24))

    let renderThread = RenderThread(this, root)

    do
        Devices.init config.AudioDevice.Value
        Window.apply_config <- this.ApplyConfig
        base.Title <- title
        base.VSync <- VSyncMode.Off
        base.CursorState <- CursorState.Hidden

    member this.ApplyConfig(config: Config) =

        let monitor =
            let monitors = Monitors.GetMonitors()
            try
                monitors.[config.Display.Value]
            with err ->
                Logging.Error (sprintf "Failed to get display info for monitor %i" config.Display.Value)
                Monitors.GetMonitorFromWindow(this)

        renderThread.RenderFrequency <- float config.FrameLimit.Value
        base.VSync <- VSyncMode.Off

        match config.WindowMode.Value with

        | WindowType.Windowed ->
            base.WindowState <- WindowState.Normal
            let width, height, resizable = config.Resolution.Value.Dimensions
            base.WindowBorder <- if resizable then WindowBorder.Resizable else WindowBorder.Fixed
            base.ClientRectangle <- new Box2i(monitor.ClientArea.Min.X, monitor.ClientArea.Min.Y, width, height)
            base.CenterWindow()

        | WindowType.Borderless ->
            base.WindowState <- WindowState.Normal
            base.ClientRectangle <- new Box2i(monitor.ClientArea.Min - Vector2i(1, 1), monitor.ClientArea.Max + Vector2i(1, 1))
            base.WindowBorder <- WindowBorder.Hidden
            base.WindowState <- WindowState.Maximized

        | WindowType.Fullscreen ->
            base.ClientRectangle <- new Box2i(monitor.ClientArea.Min - Vector2i(1, 1), monitor.ClientArea.Max + Vector2i(1, 1))
            base.WindowState <- WindowState.Fullscreen

        | WindowType.``Borderless Fullscreen`` ->
            base.WindowBorder <- WindowBorder.Hidden
            base.WindowState <- WindowState.Normal
            base.ClientRectangle <- new Box2i(monitor.ClientArea.Min - Vector2i(1, 1), monitor.ClientArea.Max + Vector2i(1, 1))

        | _ -> Logging.Error "Tried to change to invalid window mode"

    override this.OnResize e =
        base.OnResize e
        root.Sync ( fun () -> renderThread.OnResize this.ClientSize )

    override this.OnFileDrop e =
        Array.iter WindowEvents.onFileDrop.Trigger e.FileNames

    member this.Run() =
        this.OnLoad()
        this.OnResize(ResizeEventArgs(this.Size))
        
        this.Context.MakeNoneCurrent()
        renderThread.Start()

        while not (GLFW.WindowShouldClose this.WindowPtr) do
            this.ProcessInputEvents()
            GLFW.PollEvents()
            Input.poll(this.KeyboardState, this.MouseState)
            Thread.Sleep(TimeSpan.FromMilliseconds(0.5))
    
    member this.OnLoad() =
        this.ApplyConfig config
        WindowEvents.onLoad.Trigger()
        Input.init this
        Hotkeys.init()
        base.IsVisible <- true

    member this.OnUnload() =
        WindowEvents.onUnload.Trigger()