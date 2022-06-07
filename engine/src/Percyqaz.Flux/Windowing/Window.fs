namespace Percyqaz.Flux.Windowing

open System
open System.Threading
open System.Diagnostics
open OpenTK
open OpenTK.Mathematics
open OpenTK.Windowing.Desktop
open OpenTK.Windowing.Common
open OpenTK.Windowing.GraphicsLibraryFramework
open Percyqaz.Common
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Graphics
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

    let mutable resized = false
    let mutable renderFrequency = 0.0
    let between_frame_timer = Stopwatch()
    let current_frame_timer = Stopwatch()

    do
        Devices.init config.AudioDevice.Value
        Window.apply_config <- this.ApplyConfig
        base.Title <- title
        base.VSync <- VSyncMode.Off
        base.CursorState <- CursorState.Hidden

    member this.ApplyConfig(config: Config) =

        let monitor =
            let monitors = Monitors.GetMonitors()
            match Seq.tryFind (fun (x: MonitorInfo) -> x.Handle.Pointer = config.Display.Value) monitors with
            | Some m -> m
            | None ->
                Logging.Error (sprintf "Failed to get display info for monitor %i" config.Display.Value)
                Monitors.GetMonitorFromWindow(this)

        renderFrequency <- float config.FrameLimit.Value
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
        Render.resize(base.ClientSize.X, base.ClientSize.Y)
        root.Bounds <- Render.bounds
        resized <- true
        WindowEvents.onResize.Trigger()
        FBO.init()

    override this.OnFileDrop e =
        Array.iter WindowEvents.onFileDrop.Trigger e.FileNames

    member this.Run() =
        this.Context.MakeCurrent()
        this.OnLoad()
        this.OnResize(ResizeEventArgs(this.Size))
        Logging.Debug "Starting window main loop"

        between_frame_timer.Start()
        while not (GLFW.WindowShouldClose this.WindowPtr) do
            let timeUntilNextFrame = this.DispatchFrame()
            if timeUntilNextFrame > 0.0 then Thread.Sleep(Math.Floor(timeUntilNextFrame * 1000.0) |> int)

    member this.ProcessWindowEvents() =
        GLFW.PollEvents()
        // callback thingy?

    member this.DispatchFrame() =
        let between_frames = between_frame_timer.Elapsed.TotalSeconds
        let frameTime = if renderFrequency = 0.0 then 0.0 else 1.0 / renderFrequency
        current_frame_timer.Restart()

        // Update
        this.ProcessWindowEvents()
        Input.update()
        if Render.rheight > 0 then root.Update(between_frames * 1000.0, resized)
        resized <- false
        Input.finish_frame_events()
        Track.update()
        if root.ShouldExit then this.Close()

        // Draw
        Render.start()
        if Render.rheight > 0 then root.Draw()
        Render.finish()
        this.Context.SwapBuffers()

        // Timing
        between_frame_timer.Restart()
        if renderFrequency = 0.0 then 0.0 else frameTime - current_frame_timer.Elapsed.TotalSeconds
    
    member this.OnLoad() =
        this.ApplyConfig config
        Render.init(base.ClientSize.X, base.ClientSize.Y)
        FBO.init()
        Input.init this
        WindowEvents.onLoad.Trigger()
        root.Init()
        base.IsVisible <- true

    member this.OnUnload() =
        WindowEvents.onUnload.Trigger()