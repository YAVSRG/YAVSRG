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
    type Base<'T when 'T :> Widget>(item_size: float32) as this = 
        inherit StaticWidget(NodeType.Switch (fun _ -> this.WhoShouldFocus))

        let mutable filter : 'T -> bool = K true
        let mutable sort : ('T -> 'T -> int) option = None
        let mutable spacing = 0.0f
        let mutable item_size = item_size
        let mutable refresh = true
        let mutable last_selected = 0
        let children = ResizeArray<FlowItem<'T>>()

        member private this.WhoIsFocused : int option = Seq.tryFindIndex (fun c -> c.Widget.Focused) children
        member private this.WhoShouldFocus =
            if children.Count = 0 then failwithf "Tried to focus this %O with no children" this
            if last_selected >= children.Count then last_selected <- 0
            children.[last_selected].Widget

        member this.Previous() =
            match this.WhoIsFocused with
            | Some i ->
                let mutable index = (i + children.Count - 1) % children.Count
                while index <> i && not children.[index].Visible do
                    index <- (index + children.Count - 1) % children.Count
                last_selected <- index
                children.[index].Widget.Focus()
            | None -> ()

        member this.Next() =
            match this.WhoIsFocused with
            | Some i ->
                let mutable index = (i + 1) % children.Count
                while index <> i && not children.[index].Visible do
                    index <- (index + 1) % children.Count
                last_selected <- index
                children.[index].Widget.Focus()
            | None -> ()

        member this.SelectFocusedChild() =
            match this.WhoIsFocused with
            | Some i -> last_selected <- i; children.[i].Widget.Select()
            | None -> ()

        override this.OnUnfocus() =
            match this.WhoIsFocused with Some i -> last_selected <- i | None -> ()
            base.OnUnfocus()

        abstract member Navigate : unit -> unit

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
                
        member this.Clear() = children.Clear()

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

            if this.Focused then this.Navigate()

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

        override this.Navigate() =
            if (!|"up").Tapped() then this.Previous()
            elif (!|"down").Tapped() then this.Next()
            elif (!|"select").Tapped() then this.SelectFocusedChild()

    [<Sealed>]
    type LeftToRight<'T when 'T :> Widget>(item_width: float32) =
        inherit Base<'T>(item_width)
        
        let mutable content_width = 0.0f
        member this.ContentHeight = content_width

        override this.FlowContent children =
            let mutable l = 0.0f
            let mutable r = 0.0f
            for { Widget = c; Visible = visible } in children do
                if visible then
                    c.Position <- Position.Column (l, this.ItemSize)
                    l <- l + this.ItemSize + this.Spacing
                    r <- l + this.ItemSize
            content_width <- r
                    
        override this.Navigate() =
            if (!|"left").Tapped() then this.Previous()
            elif (!|"right").Tapped() then this.Next()
            elif (!|"select").Tapped() then this.SelectFocusedChild()

    [<Sealed>]
    type RightToLeft<'T when 'T :> Widget>(item_width: float32) =
        inherit Base<'T>(item_width)

        let mutable content_width = 0.0f
        member this.ContentHeight = content_width

        override this.FlowContent children =
            let mutable r = 0.0f
            let mutable l = 0.0f
            for { Widget = c; Visible = visible } in children do
                if visible then
                    c.Position <- 
                        { Left = 1.0f %- (r + this.ItemSize) ; Top = Position.min; Right = 1.0f %- r; Bottom = Position.max }
                    r <- r + this.ItemSize + this.Spacing
                    l <- r + this.ItemSize
            content_width <- l
                    
        override this.Navigate() =
            if (!|"left").Tapped() then this.Next()
            elif (!|"right").Tapped() then this.Previous()
            elif (!|"select").Tapped() then this.SelectFocusedChild()

[<Sealed>]
type ScrollContainer(child: Widget, heightFunc: unit -> float32) =
    inherit StaticWidget(NodeType.Switch (K child))

    static let SENSITIVITY = 50.0f

    let mutable scroll = 0.0f // amount in pixels to move content UP inside container

    static member Flow(child: FlowContainer.Vertical<'T>) = ScrollContainer(child, fun () -> child.ContentHeight)

    member val Margin = 0.0f with get, set

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)

        let mutable scrollby = 0.0f
        if Mouse.hover this.Bounds then scrollby <- -Mouse.scroll() * SENSITIVITY
        if this.Focused then
            let selected_bounds = (Selection.get_focused_element().Value :?> Widget).Bounds
            if selected_bounds.Bottom > this.Bounds.Bottom then
                scrollby <- scrollby + selected_bounds.Bottom - this.Bounds.Bottom
            elif this.Bounds.Top > selected_bounds.Top then
                scrollby <- scrollby - this.Bounds.Top + selected_bounds.Top
        
        // todo: detect if heightFunc() changes

        let moved = 
            if scrollby <> 0.0f then
                scroll <- Math.Max(0.0f, Math.Min(scroll + scrollby, heightFunc() - this.Bounds.Height))
                child.Position <- Position.SliceTop(heightFunc()).Translate(0.0f, -scroll).Margin(this.Margin, 0.0f)
                true
            else moved

        child.Update(elapsedTime, moved)

    override this.Draw() = 
        Stencil.create(false)
        Draw.rect this.Bounds Color.Transparent
        Stencil.draw()
        child.Draw()
        Stencil.finish()

    override this.Init(parent: Widget) =
        base.Init parent
        child.Init this
        child.Position <- Position.SliceTop(heightFunc()).Translate(0.0f, -scroll).Margin(this.Margin, 0.0f)

module SwitchContainer =

    [<AbstractClass>]
    type Base<'T when 'T :> Widget>() as this = 
        inherit StaticWidget(NodeType.Switch (fun _ -> this.WhoShouldFocus))

        let children = ResizeArray<'T>()
        let mutable last_selected = 0

        member private this.WhoIsFocused : int option = Seq.tryFindIndex (fun (c: 'T) -> c.Focused) children
        member private this.WhoShouldFocus =
            if children.Count = 0 then failwithf "Tried to focus this %O with no children" this
            if last_selected >= children.Count then last_selected <- 0
            children.[last_selected]

        member this.Previous() =
            match this.WhoIsFocused with
            | Some i ->
                let mutable index = (i + children.Count - 1) % children.Count
                while index <> i && not children.[index].Focusable do
                    index <- (index + children.Count - 1) % children.Count
                last_selected <- index
                children.[index].Focus()
            | None -> ()

        member this.Next() =
            match this.WhoIsFocused with
            | Some i ->
                let mutable index = (i + 1) % children.Count
                while index <> i && not children.[index].Focusable do
                    index <- (index + 1) % children.Count
                last_selected <- index
                children.[index].Focus()
            | None -> ()

        member this.SelectFocusedChild() =
            match this.WhoIsFocused with
            | Some i -> last_selected <- i; children.[i].Select()
            | None -> ()

        abstract member Navigate : unit -> unit
    
        override this.Draw() =
            for c in children do c.Draw()
    
        override this.Update(elapsedTime, moved) =
            base.Update(elapsedTime, moved)
            for c in children do c.Update(elapsedTime, moved)
            if this.Focused then this.Navigate()
    
        member this.Add(child: 'T) =
            children.Add child
            if this.Initialised then 
                child.Init this
    
        override this.Init(parent: Widget) =
            base.Init parent
            for c in children do
                c.Init this
    
        member this.Clear() = children.Clear()

        static member (|+) (parent: #Base<'T>, child: 'T) = parent.Add child; parent
        static member (|*) (parent: #Base<'T>, child: 'T) = parent.Add child
        
    [<Sealed>]
    type Column<'T when 'T :> Widget>() =
        inherit Base<'T>()

        override this.Navigate() =
            if (!|"up").Tapped() then this.Previous()
            elif (!|"down").Tapped() then this.Next()
            elif (!|"select").Tapped() then this.SelectFocusedChild()

    [<Sealed>]
    type Row<'T when 'T :> Widget>() =
        inherit Base<'T>()

        override this.Navigate() =
            if (!|"left").Tapped() then this.Previous()
            elif (!|"right").Tapped() then this.Next()
            elif (!|"select").Tapped() then this.SelectFocusedChild()