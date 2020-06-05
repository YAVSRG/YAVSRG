namespace Interlude

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL
open Prelude.Common
open Interlude.Render
open Interlude.Input
open Interlude.Options
open Interlude.UI

type Game(config: GameConfig) =
    inherit GameWindow(300, 300, new GraphicsMode(ColorFormat(32), 24, 8, 0))

    let screens = new ScreenContainer()

    do
        base.Title <- Utils.version
        base.VSync <- VSyncMode.Off
        //base.Cursor <- new MouseCursor.Empty

    member private this.ApplyConfig(config: GameConfig) = 
        base.TargetRenderFrequency <- config.FrameLimiter
        match config.WindowMode with
        | WindowType.WINDOWED ->
            base.WindowState <- WindowState.Normal
            let (resizable, struct (width, height)) = Options.getResolution config.Resolution
            base.WindowBorder <- if resizable then WindowBorder.Resizable else WindowBorder.Fixed
            base.ClientRectangle <- new Rectangle(0, 0, width, height)
        | WindowType.BORDERLESS ->
            base.WindowState <- WindowState.Maximized
            base.WindowBorder <- WindowBorder.Hidden
        | WindowType.FULLSCREEN ->
            base.WindowState <- WindowState.Fullscreen
        //todo: validation on config record data so this cannot happen
        | _ -> Logging.Error("Invalid window state. How did we get here?") ""

    override this.OnResize (e) =
        base.OnResize (e)
        GL.Viewport(base.ClientRectangle)
        Render.resize(float32 base.Width, float32 base.Height)

    override this.OnRenderFrame (e) =
        base.OnRenderFrame (e)
        Render.start()
        screens.Draw()
        Render.finish()
        base.SwapBuffers()

    override this.OnUpdateFrame (e) =
        base.OnUpdateFrame (e)
        screens.Update(e.Time * 1000.0, Render.bounds)
        Audio.update()
        Input.update()
        if screens.Exit then base.Exit()
    
    override this.OnLoad (e) =
        base.OnLoad(e)
        this.ApplyConfig(config)
        Render.init(float32 base.Width, float32 base.Height)
        Themes.font() |> ignore
        Input.init(this)
        Gameplay.init()

    override this.OnUnload (e) =
        Gameplay.save()
        base.OnUnload(e)