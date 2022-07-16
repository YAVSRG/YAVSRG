namespace Interlude.UI.Components

open System
open System.Drawing
open Interlude.UI
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Interlude.Utils

// TODO: flow container should be completely in charge of item positionings
type FlowContainer() =
    inherit Widget()
    let mutable spacing = 10.0f
    let mutable margin = (0.0f, 0.0f)
    let mutable contentSize = 0.0f
    let mutable scrollPos = 0.0f

    let mutable filter = K true
    let mutable sort = None
    member this.Filter with set value = filter <- value; for c in this.Children do c.Enabled <- filter c
    member this.Sort with set (comp: Comparison<Widget>) = sort <- Some comp; this.Children.Sort comp

    member this.Spacing with set(value) = spacing <- value
    // todo: margin doesn't work correctly
    member this.Margin with set (x, y) = margin <- (-x, -y)

    override this.Add (c: Widget) =
        base.Add c
        c.Enabled <- filter c
        Option.iter (fun (comp: Comparison<Widget>) -> this.Children.Sort comp) sort

    member private this.FlowContent(thisBounds: Rect) =
        let mutable vBounds = thisBounds.Translate(0.0f, -scrollPos) // todo: margin
        let t1 = vBounds.Top
        for c in this.Children do
            if c.Enabled then
                let (la, ta, ra, ba) = c.Anchors
                let wBounds =
                    if c.Initialised then Rect.Box(vBounds.Left, vBounds.Top, c.Bounds.Width, c.Bounds.Height)
                    else Rect.Create(
                            la.Position(vBounds.Left, vBounds.Right),
                            ta.Position(vBounds.Top, vBounds.Bottom),
                            ra.Position(vBounds.Left, vBounds.Right),
                            ba.Position(vBounds.Top, vBounds.Bottom)
                         )

                let pos (a: AnchorPoint) = if c.Initialised then a.MoveRelative else a.RepositionRelative
                pos la (thisBounds.Left, thisBounds.Right, wBounds.Left)
                pos ta (thisBounds.Top, thisBounds.Bottom, wBounds.Top)
                pos ra (thisBounds.Left, thisBounds.Right, wBounds.Right)
                pos ba (thisBounds.Top, thisBounds.Bottom, wBounds.Bottom)
                vBounds <- vBounds.Translate(0.0f, wBounds.Height + spacing)
        contentSize <- vBounds.Top - t1

    override this.Update(elapsedTime, bounds) =
        this.Animation.Update elapsedTime |> ignore
        this.UpdateBounds bounds
        this.FlowContent this.Bounds
        for c in this.Children do if c.Enabled then c.Update (elapsedTime, this.Bounds)
        if Mouse.hover this.Bounds then scrollPos <- scrollPos - Mouse.scroll() * 100.0f
        scrollPos <- Math.Max(0.0f, Math.Min(scrollPos, contentSize - this.Bounds.Height))

    override this.Draw() =
        Stencil.create(false)
        Draw.rect this.Bounds Color.Transparent
        Stencil.draw()
        for c in this.Children do
            if c.Initialised && c.Enabled then
                if c.Bounds.Top < this.Bounds.Bottom && c.Bounds.Bottom > this.Bounds.Top then c.Draw()
        Stencil.finish()

    /// Scrolls so that w becomes visible. w is (mostly) expected to be a child of the container but sometimes is used for sneaky workarounds
    member this.ScrollTo(w: Widget) =
        if w.Bounds.Bottom > this.Bounds.Bottom then scrollPos <- scrollPos + (w.Bounds.Bottom - this.Bounds.Bottom)
        elif w.Bounds.Top < this.Bounds.Top then scrollPos <- scrollPos - (this.Bounds.Top - w.Bounds.Top)

// provide the first tab when constructing
type TabContainer(name: string, widget: Widget) as this =
    inherit Widget()
    let mutable selectedItem = widget
    let mutable selected = name
    let mutable count = 0.0f

    let TABHEIGHT = 60.0f
    let TABWIDTH = 250.0f

    do this.AddTab(name, widget)

    member this.AddTab(name, widget) =
        { new Button((fun () -> selected <- name; selectedItem <- widget), name) with member this.Dispose() = base.Dispose(); widget.Dispose() }
            .Position (Position.Box(0.0f, 0.0f, count * TABWIDTH, 0.0f, TABWIDTH, TABHEIGHT))
        |> this.Add
        count <- count + 1.0f

    override this.Draw() =
        base.Draw()
        selectedItem.Draw()

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        selectedItem.Update(elapsedTime, this.Bounds.TrimTop TABHEIGHT)