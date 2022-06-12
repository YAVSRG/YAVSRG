namespace Percyqaz.Flux.UI

open Percyqaz.Flux.Graphics

[<AbstractClass>]
type Root() =
    inherit Widget(NodeType.None)

    override this.Position with set(value) = failwith "Position should not be set for the UI root"
    member val ShouldExit = false with get, set
    member val Animation = Animation.Group() with get

    override this.Init(parent: Widget) = failwith "Root should not have a parent"
    abstract member Init : unit -> unit
    default this.Init() =
        this.Bounds <- Viewport.bounds
        this.VisibleBounds <- Viewport.bounds

    override this.FocusTree = []

    override this.Update(elapsedTime, moved) =
        if moved then
            this.Bounds <- Viewport.bounds
            this.VisibleBounds <- Viewport.bounds

    member this.Sync action = this.Animation.Add(Animation.Action(action))