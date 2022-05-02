namespace Interlude

open OpenTK
open OpenTK.Mathematics
open OpenTK.Windowing.Desktop
open OpenTK.Windowing.Common
open Prelude.Common
open Interlude.Graphics
open Interlude.Input
open Interlude.Options
open Interlude.UI

type Game(config: GameConfig) as this =
    inherit GameWindow(GameWindowSettings.Default, NativeWindowSettings(StartVisible = false, NumberOfSamples = 24))

    let screens = Startup.init()

    do
        applyOptions <- fun () -> this.ApplyConfig config
        base.Title <- Utils.version
        base.VSync <- VSyncMode.Off
        base.CursorVisible <- false
        base.UpdateFrequency <- 120.0

    member this.ApplyConfig(config: GameConfig) =

        let monitor =
            match Monitors.TryGetMonitorInfo(config.Display.Value) with
            | true, info -> info
            | false, _ -> 
                Logging.Error (sprintf "Failed to get display info for monitor %i" config.Display.Value)
                let _, info = Monitors.TryGetMonitorInfo(base.FindMonitor()) in info

        base.RenderFrequency <- 
            match config.FrameLimit.Value with
            | FrameLimit.``30`` -> 30.0
            | FrameLimit.``60`` -> 60.0
            | FrameLimit.``120`` -> 120.0
            | FrameLimit.``240`` -> 240.0
            | FrameLimit.``480 (Recommended)`` -> 480.0
            | FrameLimit.Unlimited -> 0.0
            | FrameLimit.Vsync -> 0.0
            | _ -> 0.0
        base.VSync <- if config.FrameLimit.Value = FrameLimit.Vsync then VSyncMode.On else VSyncMode.Off

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
        FBO.init()

    override this.OnFileDrop e =
        let handle path =
            if Toolbar.Terminal.shown then
                Toolbar.Terminal.dropfile path
            elif Content.Noteskins.tryImport path [4; 7] then ()
            elif Screens.Import.FileDropHandling.tryImport path then ()
            else Logging.Warn "The file you dropped didn't look like a skin, chart or otherwise importable thing"
        e.FileNames |> Array.iter handle

    override this.OnRenderFrame e =
        base.OnRenderFrame e
        Render.start()
        if Render.rheight > 0 then screens.Draw()
        Render.finish()
        base.SwapBuffers()

    override this.OnUpdateFrame e =
        base.OnUpdateFrame e
        Input.update()
        if Render.rheight > 0 then screens.Update(e.Time * 1000.0, Render.bounds)
        elif Screen.currentType = Screen.Type.SplashScreen then screens.Update(e.Time * 1000.0, Rect.one)
        Input.absorbAll()
        Audio.update()
        if Screen.exit then base.Close()

    override this.ProcessEvents() =
        base.ProcessEvents()
        Input.poll()
    
    override this.OnLoad() =
        base.OnLoad()
        this.ApplyConfig config
        Render.init(base.ClientSize.X, base.ClientSize.Y)
        FBO.init()
        Content.init options.Theme.Value options.Noteskin.Value
        Input.init this
        Gameplay.init()
        Printerlude.init()
        base.IsVisible <- true

    override this.OnUnload() =
        Gameplay.save()
        base.OnUnload()