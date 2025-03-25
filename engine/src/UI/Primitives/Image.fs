namespace Percyqaz.Flux.UI

open System.Drawing
open Percyqaz.Flux.Graphics

[<Sealed>]
type Image(sprite: Sprite) =
    inherit StaticWidget(NodeType.None)

    member val Sprite : Sprite = sprite with get, set
    member val StretchToFill : bool = true with get, set

    override this.Draw() =
        if this.StretchToFill then
            Render.sprite this.Bounds Color.White this.Sprite
        else
            Render.sprite (Sprite.fill this.Bounds this.Sprite) Color.White this.Sprite