namespace Percyqaz.Flux.UI

open System
open System.Drawing
open System.Runtime.CompilerServices
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics

/// Container that wraps a child to allow vertical scrolling with the mouse wheel
/// Also automatically scrolls to show the selected item when navigating
[<Sealed>]
type ScrollContainer<'T when 'T :> Widget and 'T :> IHeight>(child: 'T) =
    inherit StaticWidget(NodeType.Container(K(Some child)))

    static let SENSITIVITY = 100.0f

    let mutable content_height = 0.0f
    let mutable scroll_pos = Animation.Fade 0.0f // amount in pixels to move content UP inside container
    let mutable scroll_to_child_next_frame = false

    member val Margin = 0.0f with get, set

    member this.PositionPercent = scroll_pos.Value / (content_height - this.Bounds.Height)

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        scroll_pos.Target <- Math.Max(0.0f, Math.Min(scroll_pos.Target, content_height - this.Bounds.Height))
        scroll_pos.Update elapsed_ms

        let mutable scroll_amt = 0.0f

        if Mouse.hover this.Bounds then
            if Mouse.held Mouse.RIGHT then
                let target = (Mouse.y () - this.Bounds.Top) / this.Bounds.Height * content_height
                scroll_amt <- target - scroll_pos.Target
            else
                scroll_amt <- - Mouse.scroll() * SENSITIVITY

        if this.Focused && scroll_to_child_next_frame then
            scroll_to_child_next_frame <- false

            match Selection.get_focused_element () with
            | None -> ()
            | Some child -> this.ScrollTo(child :?> Widget)

        let moved =
            if scroll_amt <> 0.0f then

                scroll_pos.Target <-
                    Math.Max(0.0f, Math.Min(scroll_pos.Target + scroll_amt, content_height - this.Bounds.Height))

                true
            else
                moved || scroll_pos.Moving

        if moved then
            child.Position <-
                Position
                    .SliceT(content_height)
                    .Translate(0.0f, -scroll_pos.Value)
                    .Shrink(this.Margin)

        if this.Focused then
            let before = Selection.get_focused_element ()
            child.Update(elapsed_ms, moved)
            let after = Selection.get_focused_element ()

            if before <> after && after <> Some child then
                scroll_to_child_next_frame <- true
        else
            child.Update(elapsed_ms, moved)

    override this.Draw() =
        if content_height > 0.0f then
            Render.stencil_create (false)
            Render.rect this.Bounds Color.Transparent
            Render.stencil_begin_draw ()
            child.Draw()
            Render.stencil_finish ()

    override this.Init(parent: Widget) =
        base.Init parent
        child.Position <- Position.Shrink(this.Margin)
        child.Init this

        content_height <- child.Height + this.Margin * 2.0f
        match child :> obj with
        | :? IResize as r -> r.OnSizeChanged <- fun () -> content_height <- child.Height + this.Margin * 2.0f
        | _ -> ()

        child.Position <-
            Position
                .SliceT(content_height)
                .Translate(0.0f, -scroll_pos.Value)
                .Shrink(this.Margin)

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse

        if Selection.get_focused_element () <> Some child then
            scroll_to_child_next_frame <- true

    member this.RemainingScrollAnimation = scroll_pos.Target - scroll_pos.Value

    member this.Scroll(amount: float32) =
        scroll_pos.Target <- Math.Max(0.0f, Math.Min(scroll_pos.Target + amount, content_height - this.Bounds.Height))

    member this.ScrollTo(widget: Widget) =
        let target =
            widget.Bounds.Translate(0.0f, scroll_pos.Value - scroll_pos.Target)

        if target.Bottom > this.Bounds.Bottom && target.Top > this.Bounds.CenterY then
            this.Scroll(target.Bottom - this.Bounds.Bottom + Style.PADDING)
        elif target.Top < this.Bounds.Top && target.Bottom < this.Bounds.CenterY then
            this.Scroll(target.Top - this.Bounds.Top - Style.PADDING)

[<Extension>]
type ScrollContainerExtensions =

    [<Extension>]
    static member Margin(container: ScrollContainer<'T>, margin: float32) : ScrollContainer<'T> =
        container.Margin <- margin
        container