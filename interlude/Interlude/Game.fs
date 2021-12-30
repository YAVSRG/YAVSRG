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
            let (resizable, struct (width, height)) = getResolution config.Resolution.Value
            base.WindowBorder <- if resizable then WindowBorder.Resizable else WindowBorder.Fixed
            base.ClientRectangle <- new Box2i(0, 0, width, height)
            base.CenterWindow()
        | WindowType.Borderless ->
            base.WindowBorder <- WindowBorder.Hidden
            base.WindowState <- WindowState.Maximized
        | WindowType.Fullscreen ->
            base.WindowState <- WindowState.Fullscreen
        | _ -> Logging.Error "Invalid window state. How did we get here?"

    override this.OnResize e =
        base.OnResize e
        Render.resize(base.ClientSize.X, base.ClientSize.Y)
        FBO.init()

    override this.OnFileDrop e =
        let handle path =
            if Content.Noteskins.tryImport path |> not then
                if Screens.Import.FileDropHandling.tryImport path |> not then
                    Logging.Warn "The file you dropped didn't look like a skin, chart or otherwise importable thing"
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
        Fonts.init()
        FBO.init()
        Content.font() |> ignore
        Input.init this
        Gameplay.init()
        base.IsVisible <- true

    override this.OnUnload() =
        Gameplay.save()
        base.OnUnload()