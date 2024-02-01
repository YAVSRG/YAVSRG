namespace Percyqaz.Flux.UI

open System
open System.Drawing
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics

/// Container that wraps a child to allow vertical scrolling with the mouse wheel
/// Also automatically scrolls to show the selected item when navigating
[<Sealed>]
type ScrollContainer<'T when 'T :> Widget and 'T :> DynamicSize>(child: 'T) =
    inherit StaticWidget(NodeType.Switch(K child))

    static let SENSITIVITY = 100.0f

    let mutable content_height = 0.0f
    let mutable scroll_pos = Animation.Fade 0.0f // amount in pixels to move content UP inside container
    let mutable refresh = false
    let mutable scroll_to_child_next_frame = false

    member val Margin = 0.0f with get, set

    member this.PositionPercent = scroll_pos.Value / (content_height - this.Bounds.Height)

    override this.Focusable = child.Focusable

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

            let selected_bounds =
                (Selection.get_focused_element().Value :?> Widget)
                    .Bounds.Translate(0.0f, scroll_pos.Value - scroll_pos.Target)

            if selected_bounds.Bottom > this.Bounds.Bottom then
                scroll_amt <- scroll_amt + selected_bounds.Bottom - this.Bounds.Bottom + Style.PADDING
            elif this.Bounds.Top > selected_bounds.Top then
                scroll_amt <- scroll_amt - this.Bounds.Top + selected_bounds.Top - Style.PADDING

        let moved =
            if scroll_amt <> 0.0f || refresh then
                refresh <- false

                scroll_pos.Target <-
                    Math.Max(0.0f, Math.Min(scroll_pos.Target + scroll_amt, content_height - this.Bounds.Height))

                true
            else
                moved || scroll_pos.Moving

        if moved then
            child.Position <-
                Position
                    .SliceTop(content_height)
                    .Translate(0.0f, -scroll_pos.Value)
                    .Margin(this.Margin)

        if this.Focused then
            let before = Selection.get_focused_element ()
            child.Update(elapsed_ms, moved)
            let after = Selection.get_focused_element ()

            if before <> after then
                scroll_to_child_next_frame <- true
        else
            child.Update(elapsed_ms, moved)

    override this.Draw() =
        if content_height > 0.0f then
            Stencil.start_stencilling (false)
            Draw.rect this.Bounds Color.Transparent
            Stencil.start_drawing ()
            child.Draw()
            Stencil.finish ()

    override this.Init(parent: Widget) =
        base.Init parent
        child.Init this

        content_height <- child.Size
        child.OnSizeChanged <- fun () -> content_height <- child.Size + this.Margin * 2.0f

        child.Position <-
            Position
                .SliceTop(content_height)
                .Translate(0.0f, -scroll_pos.Value)
                .Margin(this.Margin)

    member this.RemainingScrollAnimation = scroll_pos.Target - scroll_pos.Value

    member this.Scroll(amount: float32) =
        scroll_pos.Target <- scroll_pos.Target + amount
