namespace Percyqaz.Flux.UI

open Percyqaz.Flux.Input

[<Sealed>]
type MouseListener(on_left_click: unit -> unit) =
    inherit StaticWidget(NodeType.None)

    let mutable hover = false

    member val OnLeftClick : unit -> unit = on_left_click with get, set
    member val OnRightClick : unit -> unit = ignore with get, set
    member val OnHover : bool -> unit = ignore with get, set
    member val Floating : bool = false with get, set

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        let was_hovering = hover
        hover <- Mouse.hover (if this.Floating then this.Bounds else this.VisibleBounds)

        if was_hovering && not hover then
            this.OnHover false
        elif not was_hovering && hover && Mouse.moved_recently () then
            this.OnHover true
        elif hover then
            if Mouse.left_clicked () then
                this.OnLeftClick()

            if Mouse.right_clicked () then
                this.OnRightClick()

    override this.Draw() = ()

    static member Focus(w: Widget) =
        MouseListener(
            (fun () -> w.Select true),
            OnHover =
                fun b ->
                    if b && not w.Focused then
                        w.Focus true
                    elif not b && w.FocusedByMouse then
                        Selection.up true
        )