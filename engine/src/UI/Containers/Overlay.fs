namespace Percyqaz.Flux.UI

open Percyqaz.Flux.Graphics

[<AbstractClass>]
type Overlay(node_type: NodeType) =
    inherit Widget(node_type)

    override this.Position
        with set _ = failwith "Position can not be set for overlay components"

    override this.Init(parent: Widget) =
        base.Init parent
        this.Bounds <- Render._bounds
        this.VisibleBounds <- Render._bounds

    override this.Update(elapsed_ms, moved) =
        if moved then
            this.Bounds <- Render._bounds
            this.VisibleBounds <- Render._bounds

[<Sealed>]
type OverlayContainer(child: Widget) as this =
    inherit Overlay(NodeType.Container(fun () -> Some this.Child))

    member private this.Child = child

    override this.Init(parent) =
        base.Init parent
        child.Init this

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        child.Update(elapsed_ms, moved)

    override this.Draw() = child.Draw()
