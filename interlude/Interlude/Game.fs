namespace Interlude

open OpenTK
open OpenTK.Mathematics
open OpenTK.Windowing.Desktop
open OpenTK.Windowing.Common
open Prelude.Common
open Interlude.Render
open Interlude.Input
open Interlude.Options
open Interlude.UI

type Game(config: GameConfig) as this =
    inherit GameWindow(GameWindowSettings.Default, NativeWindowSettings(StartVisible = false, NumberOfSamples = 24, Profile = ContextProfile.Compatability))

    let screens = new ScreenContainer()

    do
        Options.applyOptions <- fun () -> this.ApplyConfig(config)
        base.Title <- Utils.version
        base.VSync <- VSyncMode.Off
        base.CursorVisible <- false
        base.UpdateFrequency <- 120.0
        base.IsVisible <- true

    member this.ApplyConfig(config: GameConfig) =
        base.RenderFrequency <- config.FrameLimiter.Get()
        match config.WindowMode.Get() with
        | WindowType.WINDOWED ->
            base.WindowState <- WindowState.Normal
            let (resizable, struct (width, height)) = Options.getResolution <| config.Resolution.Get()
            base.WindowBorder <- if resizable then WindowBorder.Resizable else WindowBorder.Fixed
            base.ClientRectangle <- new Box2i(0, 0, width, height)
            base.CenterWindow()
        | WindowType.BORDERLESS ->
            base.WindowBorder <- WindowBorder.Hidden
            base.WindowState <- WindowState.Maximized
        | WindowType.FULLSCREEN ->
            base.WindowState <- WindowState.Fullscreen
        | _ -> Logging.Error("Invalid window state. How did we get here?") ""

    override this.OnResize(e) =
        base.OnResize(e)
        Render.resize(base.ClientSize.X, base.ClientSize.Y)
        FBO.init()

    override this.OnFileDrop(e) =
        e.FileNames |> Array.iter FileDropHandling.import

    override this.OnRenderFrame(e) =
        base.OnRenderFrame(e)
        Render.start()
        screens.Draw()
        Render.finish()
        base.SwapBuffers()

    override this.OnUpdateFrame(e) =
        base.OnUpdateFrame(e)
        screens.Update(e.Time * 1000.0, Render.bounds)
        Input.update()
        Audio.update()
        if screens.Exit then base.Close()
    
    override this.OnLoad() =
        base.OnLoad()
        this.ApplyConfig(config)
        Render.init(base.ClientSize.X, base.ClientSize.Y)
        FBO.init()
        Themes.font() |> ignore
        Input.init(this)
        Gameplay.init()

    override this.OnUnload() =
        Gameplay.save()
        base.OnUnload()