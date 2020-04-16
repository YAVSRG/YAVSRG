namespace Interlude

open OpenTK
open OpenTK.Graphics

module Game = 
    let version = "v0.4.0"

type Game() =
    inherit GameWindow(300, 300, new GraphicsMode(ColorFormat(32), 24, 8, 0, ColorFormat(0)))

    do
        base.Title <- "Interlude " + Game.version

    override this.OnRenderFrame (e) =
        base.OnRenderFrame (e)