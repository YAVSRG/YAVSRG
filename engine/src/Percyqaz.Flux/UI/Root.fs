namespace Percyqaz.Flux.UI

open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input

[<AbstractClass>]
type Root() =
    inherit Widget(NodeType.None)

    override this.Position with set(value) = failwith "root position is tied to the bounds of the screen"
    member val ShouldExit = false with get, set
    member val Animation = Animation.Group() with get

    override this.Init(parent: Widget) = failwith "call .Init() instead"
    abstract member Init : unit -> unit
    default this.Init() =
        this.Bounds <- Viewport.bounds
        this.VisibleBounds <- Viewport.bounds

    override this.FocusTree = []

    override this.Update(elapsedTime, moved) =
        if moved then
            this.Bounds <- Viewport.bounds
            this.VisibleBounds <- Viewport.bounds
        if (!|"exit").Tapped() then Selection.up()

    member this.Sync action = this.Animation.Add(Animation.Action(action))