namespace Interlude

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL
open Prelude.Common
open Interlude.Render
open Interlude.Input
open Interlude.Options
open Interlude.UI

module Game = 
    let version = "v0.4.0"

type Game(config: GameConfig) =
    inherit GameWindow(300, 300, new GraphicsMode(ColorFormat(32), 24, 8, 0))

    let screens = new ScreenContainer()

    do
        base.Title <- "Interlude " + Game.version
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
        Render.resize(float base.Width, float base.Height)

    override this.OnRenderFrame (e) =
        base.OnRenderFrame (e)
        Render.start()
        screens.Draw()
        Render.finish()
        base.SwapBuffers()

    override this.OnUpdateFrame (e) =
        base.OnUpdateFrame (e)
        Input.update()
        screens.Update(e.Time * 1000.0, Rect.create 0.0f 0.0f (float32 base.Width) (float32 base.Height))
        if screens.Exit then base.Exit()
    
    override this.OnLoad (e) =
        base.OnLoad(e)
        this.ApplyConfig(config)
        Render.init(float base.Width, float base.Height)
        Input.init(this)

    override this.OnUnload (e) =
        base.OnUnload(e)