namespace Percyqaz.Flux.UI

open System
open System.Drawing
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics

/// Container that wraps a child to allow vertical scrolling with the mouse wheel
/// Also automatically scrolls to show the selected item when navigating
[<Sealed>]
type ScrollContainer(child: Widget, contentHeight: float32) =
    inherit StaticWidget(NodeType.Switch (K child))

    static let SENSITIVITY = 100.0f

    let mutable scroll_pos = Animation.Fade 0.0f // amount in pixels to move content UP inside container
    let mutable contentHeight = contentHeight
    let mutable refresh = false
    let mutable scroll_to_child_next_frame = false

    static member Flow(child: FlowContainer.Vertical<'T>) =
        let sc = ScrollContainer(child, 0.0f)
        child.ContentHeightChanged.Add(sc.set_ContentHeight)
        sc
        
    static member Grid(child: GridContainer<'T>) =
        let sc = ScrollContainer(child, 0.0f)
        child.ContentHeightChanged.Add(sc.set_ContentHeight)
        sc

    member val Margin = 0.0f with get, set
    member this.ContentHeight with set(value) = contentHeight <- value; refresh <- true
    member this.PositionPercent = scroll_pos.Value / (contentHeight - this.Bounds.Height)

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        scroll_pos.Target <- Math.Max(0.0f, Math.Min(scroll_pos.Target, contentHeight - this.Bounds.Height))
        scroll_pos.Update elapsedTime

        let mutable scrollby = 0.0f
        if Mouse.hover this.Bounds then
            if Mouse.held Mouse.RIGHT then
                let target = (Mouse.y() - this.Bounds.Top) / this.Bounds.Height * contentHeight
                scrollby <- target - scroll_pos.Target
            else scrollby <- -Mouse.scroll() * SENSITIVITY
        if this.Focused && scroll_to_child_next_frame then
            scroll_to_child_next_frame <- false
            let selected_bounds = (Selection.get_focused_element().Value :?> Widget).Bounds.Translate(0.0f, scroll_pos.Value - scroll_pos.Target)
            if selected_bounds.Bottom > this.Bounds.Bottom then
                scrollby <- scrollby + selected_bounds.Bottom - this.Bounds.Bottom + Style.padding
            elif this.Bounds.Top > selected_bounds.Top then
                scrollby <- scrollby - this.Bounds.Top + selected_bounds.Top - Style.padding

        let moved = 
            if scrollby <> 0.0f || refresh then
                refresh <- false
                scroll_pos.Target <- Math.Max(0.0f, Math.Min(scroll_pos.Target + scrollby, contentHeight - this.Bounds.Height))
                true
            else moved || scroll_pos.Moving

        if moved then child.Position <- Position.SliceTop(contentHeight).Translate(0.0f, -scroll_pos.Value).Margin(this.Margin)
        if this.Focused then
            let before = Selection.get_focused_element()
            child.Update(elapsedTime, moved)
            let after = Selection.get_focused_element()
            if before <> after && abs (scroll_pos.Target - scroll_pos.Value) < 10.0f then scroll_to_child_next_frame <- true
        else child.Update(elapsedTime, moved)

    override this.Draw() = 
        Stencil.create(false)
        Draw.rect this.Bounds Color.Transparent
        Stencil.draw()
        child.Draw()
        Stencil.finish()

    override this.Init(parent: Widget) =
        base.Init parent
        child.Init this
        child.Position <- Position.SliceTop(contentHeight).Translate(0.0f, -scroll_pos.Value).Margin(this.Margin)

    member this.RemainingScrollAnimation = scroll_pos.Target - scroll_pos.Value
    member this.Scroll(amount: float32) =
        scroll_pos.Target <- scroll_pos.Target + amount