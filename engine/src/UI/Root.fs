namespace Percyqaz.Flux.UI

open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Utils

[<AutoOpen>]
module Root =
    let internal ROOT_ANIMATION = Animation.Group()

    /// Defers an action to the next update cycle of the UI thread
    /// Used for deferring an action to the main thread from code in other threads
    /// Also used for deferring an action until after the frame where doing it inline would be incorrect
    let defer (action: unit -> unit) =
        ROOT_ANIMATION.Add(Animation.Action action)

    let add_to_update_loop (action: unit -> unit) =
        ROOT_ANIMATION.Add(Animation.ActionLoop action)

    let inline ensure_ui_thread action =
        if is_ui_thread () then action () else defer action

[<AbstractClass>]
type Root() =
    inherit Widget(NodeType.None)

    override this.Position
        with set (value) = failwith "root position is tied to the bounds of the screen"

    member val ShouldExit = false with get, set

    override this.Init(parent: Widget) = failwith "call .Init() instead"
    abstract member Init: unit -> unit

    default this.Init() =
        this.Bounds <- Render._bounds
        this.VisibleBounds <- Render._bounds
        ROOT_ANIMATION.Add Palette.accent_color

    override this.FocusTree = []

    override this.Update(elapsed_ms, moved) =
        if moved then
            this.Bounds <- Render._bounds
            this.VisibleBounds <- Render._bounds
