namespace Percyqaz.Flux.UI

open Percyqaz.Flux.Graphics

[<AbstractClass>]
type Root() =
    inherit Widget()

    override this.Position with set(value) = failwith "Position should not be set for the UI root"
    member val ShouldExit = false with get, set

    override this.Init(parent: Widget) = failwith "Root should not have a parent"
    abstract member Init : unit -> unit
    default this.Init() =
        this.Bounds <- Render.bounds
        this.VisibleBounds <- Render.bounds

    override this.Update(elapsedTime, moved) =
        if moved then
            this.Bounds <- Render.bounds
            this.VisibleBounds <- Render.bounds