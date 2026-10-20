namespace Percyqaz.Flux.UI

open Percyqaz.Flux.Graphics

[<AbstractClass>]
type Overlay(node_type: NodeType) =
    inherit Widget(node_type)

    override this.Position
        with set _ = failwith "Position cannot be set for overlay components"

    override this.Init(parent: Widget) : unit =
        base.Init(parent)
        this.Bounds <- Render._bounds
        this.VisibleBounds <- Render._bounds

    override this.Update(_elapsed_ms: float, moved: bool) : unit =
        if moved then
            this.Bounds <- Render._bounds
            this.VisibleBounds <- Render._bounds

[<Sealed>]
type OverlayContainer(child: Widget) as this =
    inherit Overlay(NodeType.Container(fun () -> Some this.Child))

    member private this.Child = child

    override this.Init(parent: Widget) : unit =
        base.Init(parent)
        child.Init(this)

    override this.Update(elapsed_ms: float, moved: bool) : unit =
        base.Update(elapsed_ms, moved)
        child.Update(elapsed_ms, moved)

    override this.Draw() : unit = child.Draw()
