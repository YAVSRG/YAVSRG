namespace Percyqaz.Flux.Windowing

open OpenTK
open OpenTK.Mathematics
open OpenTK.Windowing.Desktop
open OpenTK.Windowing.Common
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
    inherit GameWindow(GameWindowSettings(UpdateFrequency = 120.0), NativeWindowSettings(StartVisible = false, NumberOfSamples = 24))

    let mutable resized = false

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

        base.RenderFrequency <- float config.FrameLimit.Value
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

    override this.OnRenderFrame e =
        base.OnRenderFrame e
        Render.start()
        if Render.rheight > 0 then root.Draw()
        Render.finish()
        base.SwapBuffers()

    override this.OnUpdateFrame e =
        base.OnUpdateFrame e
        Input.poll() // todo: 1000hz polling
        Input.update()
        if Render.rheight > 0 then root.Update(e.Time * 1000.0, resized)
        resized <- false
        Input.absorbAll()
        Track.update()
        if root.ShouldExit then base.Close()
    
    override this.OnLoad() =
        base.OnLoad()
        this.ApplyConfig config
        Render.init(base.ClientSize.X, base.ClientSize.Y)
        FBO.init()
        Input.init this
        WindowEvents.onLoad.Trigger()
        root.Init()
        base.IsVisible <- true

    override this.OnUnload() =
        WindowEvents.onUnload.Trigger()
        base.OnUnload()