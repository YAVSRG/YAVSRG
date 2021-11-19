namespace Interlude.UI.Components

open System
open System.Drawing
open Interlude.UI
open Interlude.Input
open Interlude.Utils
open Interlude.Graphics

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
    //todo: margin doesn't work correctly
    member this.Margin with set (x, y) = margin <- (-x, -y)

    override this.Add (c: Widget) =
        base.Add c
        c.Enabled <- filter c
        Option.iter (fun (comp: Comparison<Widget>) -> this.Children.Sort comp) sort

    member private this.FlowContent(thisBounds) =
        let mutable vBounds = thisBounds |> Rect.expand margin |> Rect.translate(0.0f, -scrollPos)
        let struct (left, top, right, bottom) = thisBounds
        let struct (_, t1, _, _) = vBounds
        for c in this.Children do
            if c.Enabled then
                let (la, ta, ra, ba) = c.Anchors
                let struct (l, t, r, b) = vBounds
                let struct (lb, tb, rb, bb) =
                    if c.Initialised then Rect.createWH l t (Rect.width c.Bounds) (Rect.height c.Bounds)
                    else Rect.create (la.Position(l, r)) (ta.Position(t, b)) (ra.Position(l, r)) (ba.Position(t, b))
                let pos (a: AnchorPoint) = if c.Initialised then a.MoveRelative else a.RepositionRelative
                pos la (left, right, lb); pos ta (top, bottom, tb); pos ra (left, right, rb); pos ba (top, bottom, bb)
                vBounds <- Rect.translate(0.0f, bb - tb + spacing) vBounds
        let struct (_, t2, _, _) = vBounds
        contentSize <- t2 - t1

    override this.Update(elapsedTime, bounds) =
        this.Animation.Update elapsedTime |> ignore
        this.UpdateBounds bounds
        this.FlowContent this.Bounds
        for c in this.Children do if c.Enabled then c.Update (elapsedTime, this.Bounds)
        if Mouse.Hover this.Bounds then scrollPos <- scrollPos - Mouse.Scroll() * 100.0f
        scrollPos <- Math.Max(0.0f, Math.Min(scrollPos, contentSize - Rect.height this.Bounds))

    override this.Draw() =
        Stencil.create(false)
        Draw.rect this.Bounds Color.Transparent Sprite.Default
        Stencil.draw()
        let struct (_, top, _, bottom) = this.Bounds
        for c in this.Children do
            if c.Initialised && c.Enabled then
                let struct (_, t, _, b) = c.Bounds
                if t < bottom && b > top then c.Draw()
        Stencil.finish()

    //scrolls so that w becomes visible. w is (mostly) expected to be a child of the container but sometimes is used for sneaky workarounds
    member this.ScrollTo(w: Widget) =
        let struct (_, top, _, bottom) = this.Bounds
        let struct (_, ctop, _, cbottom) = w.Bounds
        if cbottom > bottom then scrollPos <- scrollPos + (cbottom - bottom)
        elif ctop < top then scrollPos <- scrollPos - (top - ctop)

//provide the first tab when constructing
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
        |> positionWidget(count * TABWIDTH, 0.0f, 0.0f, 0.0f, (count + 1.0f) * TABWIDTH, 0.0f, TABHEIGHT, 0.0f)
        |> this.Add
        count <- count + 1.0f

    override this.Draw() =
        base.Draw()
        selectedItem.Draw()

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        selectedItem.Update(elapsedTime, Rect.trimTop TABHEIGHT this.Bounds)