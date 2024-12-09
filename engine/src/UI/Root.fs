namespace Percyqaz.Flux.UI

open Percyqaz.Flux.Windowing
open Percyqaz.Flux.Graphics

[<AbstractClass>]
type Root() =
    inherit Widget(NodeType.None)

    override this.Position
        with set (value) = failwith "root position is tied to the bounds of the screen"

    override this.Init(parent: Widget) = failwith "call .Init() instead"
    abstract member Init: unit -> unit

    default this.Init() =
        this.Bounds <- Render._bounds
        this.VisibleBounds <- Render._bounds

    override this.FocusTree = []

    override this.Update(elapsed_ms, moved) =
        Palette.accent_color.Update elapsed_ms
        if moved then
            this.Bounds <- Render._bounds
            this.VisibleBounds <- Render._bounds

    interface UIEntryPoint with
        member this.Init() = this.Init()
        member this.Update(elapsed_ms, moved) = this.Update(elapsed_ms, moved)
        member this.Draw() = this.Draw()
