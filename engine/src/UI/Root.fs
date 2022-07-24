namespace Percyqaz.Flux.UI

open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input

[<AutoOpen>]
module Root =
    let internal ROOT_ANIMATION = Animation.Group()

    let sync (action: unit -> unit) = ROOT_ANIMATION.Add(Animation.Action action)
[<AbstractClass>]
type Root() =
    inherit Widget(NodeType.None)

    override this.Position with set(value) = failwith "root position is tied to the bounds of the screen"
    member val ShouldExit = false with get, set

    override this.Init(parent: Widget) = failwith "call .Init() instead"
    abstract member Init : unit -> unit
    default this.Init() =
        this.Bounds <- Viewport.bounds
        this.VisibleBounds <- Viewport.bounds

    override this.FocusTree = []

    override this.Update(elapsedTime, moved) =
        Style.accentColor.Update(elapsedTime)
        if moved then
            this.Bounds <- Viewport.bounds
            this.VisibleBounds <- Viewport.bounds
        if (!|"exit").Tapped() then Selection.up()