namespace Percyqaz.Flux.UI

open System
open System.Drawing
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics

module FlowContainer =

    type FlowItem<'T when 'T :> Widget> =
        {
            Widget: 'T
            mutable Visible: bool
        }

    [<AbstractClass>]
    type Base<'T when 'T :> Widget>(item_size: float32) = 
        inherit StaticWidget()

        let mutable filter : 'T -> bool = K true
        let mutable sort : ('T -> 'T -> int) option = None
        let mutable spacing = 0.0f
        let mutable item_size = item_size
        let mutable refresh = true
        let children = ResizeArray<FlowItem<'T>>()

        member this.Filter 
            with set value = 
                filter <- value
                for c in children do c.Visible <- filter c.Widget
                refresh <- true

        member this.Sort
            with set (comp: 'T -> 'T -> int) = 
                sort <- Some comp
                children.Sort (fun { Widget = a } { Widget = b } -> comp a b)
                refresh <- true

        member this.Spacing
            with get() = spacing
            and set(value) =
                spacing <- value
                refresh <- true
                
        member this.ItemSize
            with get() = item_size
            and set(value) =
                item_size <- value
                refresh <- true
        
        override this.Draw() =
            for { Widget = c; Visible = visible } in children do
                if visible && c.VisibleBounds.Visible then c.Draw()
        
        override this.Update(elapsedTime, moved) =
            base.Update(elapsedTime, moved)

            let moved = 
                if refresh then
                    refresh <- false
                    this.FlowContent children
                    true
                else moved

            for { Widget = c; Visible = visible } in children do
                if visible && (moved || c.VisibleBounds.Visible) then
                    c.Update(elapsedTime, moved)

        abstract member FlowContent : ResizeArray<FlowItem<'T>> -> unit
        
        member this.Add(child: 'T) =
            children.Add { Widget = child; Visible = filter child }
            match sort with
            | Some comp -> children.Sort (fun { Widget = a } { Widget = b } -> comp a b)
            | None -> ()
            if this.Initialised then 
                child.Init this
                refresh <- true
        
        override this.Init(parent: Widget) =
            base.Init parent
            this.FlowContent children
            for { Widget = c } in children do
                c.Init this
        
        static member (|+) (parent: #Base<'T>, child: 'T) = parent.Add child; parent
        static member (|*) (parent: #Base<'T>, child: 'T) = parent.Add child

    [<Sealed>]
    type Vertical<'T when 'T :> Widget>(item_height: float32) =
        inherit Base<'T>(item_height)

        let mutable content_height = 0.0f

        member this.ContentHeight = content_height

        override this.FlowContent children =
            let mutable t = 0.0f
            let mutable b = 0.0f
            for { Widget = c; Visible = visible } in children do
                if visible then
                    c.Position <- Position.Row (t, this.ItemSize)
                    t <- t + this.ItemSize + this.Spacing
                    b <- t + this.ItemSize
            content_height <- b

    [<Sealed>]
    type LeftToRight<'T when 'T :> Widget>(item_width: float32) =
        inherit Base<'T>(item_width)

        override this.FlowContent children =
            let mutable l = 0.0f
            let mutable r = 0.0f
            for { Widget = c; Visible = visible } in children do
                if visible then
                    c.Position <- Position.Column (l, this.ItemSize)
                    l <- l + this.ItemSize + this.Spacing
                    r <- l + this.ItemSize

[<Sealed>]
type ScrollContainer(child: Widget, heightFunc: unit -> float32) =
    inherit StaticWidget()

    static let SENSITIVITY = 50.0f

    let mutable scroll = 0.0f // amount in pixels to move content UP inside container

    static member Flow(child: FlowContainer.Vertical<'T>) = ScrollContainer(child, fun () -> child.ContentHeight)

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        let moved = 
            if Mouse.hover this.Bounds then
                let ms = Mouse.scroll()
                if ms <> 0.0f then
                    scroll <- scroll - ms * SENSITIVITY
                    scroll <- Math.Max(0.0f, Math.Min(scroll, heightFunc() - this.Bounds.Height))
                    child.Position <- Position.SliceTop(heightFunc()).Translate(0.0f, -scroll)
                    true
                else moved
            else moved
        child.Update(elapsedTime, moved)

    override this.Draw() = 
        Stencil.create(false)
        Draw.rect this.Bounds Color.Transparent Sprite.Default
        Stencil.draw()
        child.Draw()
        Stencil.finish()

    override this.Init(parent: Widget) =
        base.Init parent
        child.Init this