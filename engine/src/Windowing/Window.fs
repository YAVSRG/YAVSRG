namespace Percyqaz.Flux.Windowing

open System
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

    let mutable apply_config = None
    let mutable monitors = [||]

[<Sealed>]
type Window(config: Config, title: string, root: Root) as this =
    inherit NativeWindow(NativeWindowSettings(StartVisible = false, NumberOfSamples = 24))

    let renderThread = RenderThread(this, config.AudioDevice.Value, root, WindowEvents.afterInit.Trigger)

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
            try
                monitor_list.[config.Display.Value]
            with err ->
                Logging.Error (sprintf "Failed to get display info for monitor %i" config.Display.Value)
                Monitors.GetMonitorFromWindow(this)

        renderThread.RenderFrequency <- float config.FrameLimit.Value

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
        if e.Height <> 0 then
            sync ( fun () -> renderThread.OnResize this.ClientSize )

    override this.OnFileDrop e =
        Array.iter WindowEvents.onFileDrop.Trigger e.FileNames

    member this.Run() =
        this.OnLoad()
        this.OnResize(ResizeEventArgs(this.Size))
        
        this.Context.MakeNoneCurrent()
        renderThread.Start()

        while not (GLFW.WindowShouldClose this.WindowPtr) do
            if Window.apply_config.IsSome then
                this.ApplyConfig Window.apply_config.Value
                Window.apply_config <- None
            this.ProcessInputEvents()
            GLFW.PollEvents()
            InputThread.poll(this.KeyboardState, this.MouseState)
            Thread.Sleep(TimeSpan.FromMilliseconds(0.5))

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