namespace Percyqaz.Flux.UI

open Percyqaz.Flux.Graphics

[<AutoOpen>]
module Root =
    let internal ROOT_ANIMATION = Animation.Group()

    let sync (action: unit -> unit) =
        ROOT_ANIMATION.Add(Animation.Action action)

    let sync_forever (action: unit -> unit) =
        ROOT_ANIMATION.Add(Animation.ActionLoop action)

[<AbstractClass>]
type Root() =
    inherit Widget(NodeType.None)

    override this.Position
        with set (value) = failwith "root position is tied to the bounds of the screen"

    member val ShouldExit = false with get, set

    override this.Init(parent: Widget) = failwith "call .Init() instead"
    abstract member Init: unit -> unit

    default this.Init() =
        this.Bounds <- Viewport.bounds
        this.VisibleBounds <- Viewport.bounds
        ROOT_ANIMATION.Add Palette.accent_color

    override this.FocusTree = []

    override this.Update(elapsed_ms, moved) =
        if moved then
            this.Bounds <- Viewport.bounds
            this.VisibleBounds <- Viewport.bounds
