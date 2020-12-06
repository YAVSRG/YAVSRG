namespace Interlude

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL
open Prelude.Common
open Interlude.Render
open Interlude.Input
open Interlude.Options
open Interlude.UI

type Game(config: GameConfig) as this =
    inherit GameWindow(300, 300, new GraphicsMode(ColorFormat(32), 24, 8, 0))

    let screens = new ScreenContainer()

    do
        Options.applyOptions <- fun () -> this.ApplyConfig(config)
        base.Title <- Utils.version
        base.VSync <- VSyncMode.Off
        //base.Cursor <- new MouseCursor.Empty

    member this.ApplyConfig(config: GameConfig) =
        base.TargetRenderFrequency <- config.FrameLimiter.Get()
        match config.WindowMode.Get() with
        | WindowType.WINDOWED ->
            base.WindowState <- WindowState.Normal
            let (resizable, struct (width, height)) = Options.getResolution <| config.Resolution.Get()
            base.WindowBorder <- if resizable then WindowBorder.Resizable else WindowBorder.Fixed
            base.ClientRectangle <- new Rectangle(0, 0, width, height)
        | WindowType.BORDERLESS ->
            base.WindowState <- WindowState.Maximized
            base.WindowBorder <- WindowBorder.Hidden
        | WindowType.FULLSCREEN ->
            base.WindowState <- WindowState.Fullscreen
        //todo: validation on config record data so this cannot happen
        | _ -> Logging.Error("Invalid window state. How did we get here?") ""

    override this.OnResize(e) =
        base.OnResize (e)
        GL.Viewport(base.ClientRectangle)
        Render.resize(base.Width, base.Height)
        FBO.init()

    override this.OnFileDrop(e) =
        FileDropHandling.import(e.FileName)

    override this.OnRenderFrame(e) =
        base.OnRenderFrame (e)
        Render.start()
        screens.Draw()
        Render.finish()
        base.SwapBuffers()

    override this.OnUpdateFrame(e) =
        base.OnUpdateFrame(e)
        screens.Update(e.Time * 1000.0, Render.bounds)
        Audio.update()
        Input.update()
        if screens.Exit then base.Exit()
    
    override this.OnLoad(e) =
        base.OnLoad(e)
        this.ApplyConfig(config)
        Render.init(base.Width, base.Height)
        FBO.init()
        Themes.font() |> ignore
        Input.init(this)
        Gameplay.init()

    override this.OnUnload(e) =
        Gameplay.save()
        base.OnUnload(e)