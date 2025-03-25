namespace Percyqaz.Flux.UI

open Percyqaz.Flux.Input
open System.Runtime.CompilerServices

[<Sealed>]
type MouseListener() =
    inherit StaticWidget(NodeType.None)

    let mutable hover = false

    member val OnLeftClick : unit -> unit = ignore with get, set
    member val OnRightClick : unit -> unit = ignore with get, set
    member val OnHover : bool -> unit = ignore with get, set
    /// By default, the listened area is restricted to the area that is also inside all parents of the component
    /// This means elements that are partly obscured can't be interacted with outside the bounds of their parent
    /// Example: Scroll container with elements such that one is half-in and half-out; Only the half that is in can be interacted with
    /// Setting `Floating` to true will turn this behaviour off so the entire bounding box is interactable, all the time
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

[<Extension>]
type MouseListenerExtensions =

    [<Extension>]
    static member OnLeftClick (listener: MouseListener, action: unit -> unit) : MouseListener =
        listener.OnLeftClick <- action
        listener

    [<Extension>]
    static member OnRightClick (listener: MouseListener, action: unit -> unit) : MouseListener =
        listener.OnRightClick <- action
        listener

    [<Extension>]
    static member OnHover (listener: MouseListener, action: bool -> unit) : MouseListener =
        listener.OnHover <- action
        listener

    [<Extension>]
    static member Floating (listener: MouseListener, floating: bool) : MouseListener =
        listener.Floating <- floating
        listener

    [<Extension>]
    static member Floating (listener: MouseListener) : MouseListener =
        listener.Floating <- true
        listener

    [<Extension>]
    static member FocusOnHover (listener: MouseListener, widget: #Widget) : MouseListener =
        listener.OnHover <-
            fun now_hovering ->
                if now_hovering && not widget.Focused then
                    widget.Focus true
                elif not now_hovering && widget.FocusedByMouse && not widget.Selected then
                    Selection.up true
        listener

    [<Extension>]
    static member SelectOnClick (listener: MouseListener, widget: #Widget) : MouseListener =
        listener.OnLeftClick <- fun () -> widget.Select true
        listener

    [<Extension>]
    static member SelectOnClick (listener: MouseListener, widget: #Widget, also_on_click: unit -> unit) : MouseListener =
        listener.OnLeftClick <- fun () -> widget.Select true; also_on_click()
        listener

    [<Extension>]
    static member Button (listener: MouseListener, widget: #Widget) : MouseListener =
        listener
            .SelectOnClick(widget)
            .FocusOnHover(widget)