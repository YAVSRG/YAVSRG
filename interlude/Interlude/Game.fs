namespace Interlude

open System
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
    inherit GameWindow(GameWindowSettings.Default, NativeWindowSettings(StartVisible = false, NumberOfSamples = 24, Profile = ContextProfile.Compatability))

    let screens = new ScreenContainer()

    let sw = System.Diagnostics.Stopwatch()
    let mutable (_gen0, _gen1, _gen2) = (0, 0, 0)

    do
        Options.applyOptions <- fun () -> this.ApplyConfig config
        base.Title <- Utils.version
        base.VSync <- VSyncMode.Off
        base.CursorVisible <- false
        base.UpdateFrequency <- 120.0
        base.IsVisible <- true

    member this.ApplyConfig(config: GameConfig) =
        base.RenderFrequency <- config.FrameLimiter.Value
        match config.WindowMode.Value with
        | WindowType.WINDOWED ->
            base.WindowState <- WindowState.Normal
            let (resizable, struct (width, height)) = Options.getResolution config.Resolution.Value
            base.WindowBorder <- if resizable then WindowBorder.Resizable else WindowBorder.Fixed
            base.ClientRectangle <- new Box2i(0, 0, width, height)
            base.CenterWindow()
        | WindowType.BORDERLESS ->
            base.WindowBorder <- WindowBorder.Hidden
            base.WindowState <- WindowState.Maximized
        | WindowType.FULLSCREEN ->
            base.WindowState <- WindowState.Fullscreen
        | _ -> Logging.Error("Invalid window state. How did we get here?")

    override this.OnResize e =
        base.OnResize e
        Render.resize(base.ClientSize.X, base.ClientSize.Y)
        FBO.init()

    override this.OnFileDrop e =
        e.FileNames |> Array.iter FileDropHandling.import

    override this.OnRenderFrame e =
        base.OnRenderFrame e
        sw.Restart()
        Render.start()
        if Render.rheight > 0 then screens.Draw()
        Render.finish()
        sw.Stop()
        base.SwapBuffers()
        Utils.RenderPerformance.frame (float32 sw.Elapsed.TotalMilliseconds) (float32 e.Time * 1000.0f)

    override this.OnUpdateFrame e =
        sw.Restart()
        base.OnUpdateFrame e
        Input.update()
        if Render.rheight > 0 then screens.Update(e.Time * 1000.0, Render.bounds)
        Input.absorbAll()
        Audio.update()
        sw.Stop()
        let (gen0, gen1, gen2) = (GC.CollectionCount(0), GC.CollectionCount(1), GC.CollectionCount(2))
        Utils.RenderPerformance.update (float32(gen0 - _gen0) * 30.0f) (float32(gen1 - _gen1) * 50.0f + float32(gen2 - _gen2) * 200.0f)//(float32 sw.Elapsed.TotalMilliseconds) (float32 e.Time * 1000.0f)
        _gen0 <- gen0
        _gen1 <- gen1
        _gen2 <- gen2
        if screens.Exit then base.Close()

    override this.ProcessEvents() =
        base.ProcessEvents()
        Input.poll()
    
    override this.OnLoad() =
        base.OnLoad()
        this.ApplyConfig config
        Render.init(base.ClientSize.X, base.ClientSize.Y)
        FBO.init()
        Themes.font() |> ignore
        Input.init this
        Gameplay.init()

    override this.OnUnload() =
        Gameplay.save()
        base.OnUnload()