namespace Percyqaz.Flux.UI

open System
open System.Drawing
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics

/// Container that automatically positions its contents stacked in Vertical/Horizontal arrangements
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

        member val Navigation = true with get, set
                
        member this.Clear() = children.Clear()

        override this.Draw() =
            for { Widget = c; Visible = visible } in children do
                if visible && c.VisibleBounds.Visible then c.Draw()
        
        override this.Update(elapsedTime, moved) =
            base.Update(elapsedTime, moved || refresh)

            let moved = 
                if refresh then
                    refresh <- false
                    this.FlowContent children
                    true
                else moved

            for { Widget = c; Visible = visible } in children do
                if visible && (moved || c.VisibleBounds.Visible) then
                    c.Update(elapsedTime, moved)

            if this.Navigation && this.Focused then this.Navigate()

        abstract member FlowContent : ResizeArray<FlowItem<'T>> -> unit
        
        member this.Add(child: 'T) =
            children.Add { Widget = child; Visible = filter child }
            match sort with
            | Some comp -> children.Sort (fun { Widget = a } { Widget = b } -> comp a b)
            | None -> ()
            if this.Initialised then 
                child.Init this
                refresh <- true

        member this.Remove(child: 'T) =
            match Seq.tryFind (fun { Widget = c } -> Object.ReferenceEquals(c, child)) children with
            | Some x ->
                children.Remove x |> ignore
                refresh <- true
            | None -> Logging.Error(sprintf "%O is not in flow container %O, can't remove" child this)
        
        override this.Init(parent: Widget) =
            base.Init parent
            this.FlowContent children
            for { Widget = c } in children do
                c.Init this

        member this.Iter(f: 'T -> unit) =
            for { Widget = c } in children do
                f c
        
        static member (|+) (parent: #Base<'T>, child: 'T) = parent.Add child; parent
        static member (|*) (parent: #Base<'T>, child: 'T) = parent.Add child

    [<Sealed>]
    type Vertical<'T when 'T :> Widget>(item_height: float32) =
        inherit Base<'T>(item_height)

        let mutable content_height = 0.0f
        let contentChangeEvent = Event<float32>()
        member this.ContentHeightChanged = contentChangeEvent.Publish

        override this.FlowContent children =
            let mutable t = 0.0f
            let mutable b = 0.0f
            for { Widget = c; Visible = visible } in children do
                if visible then
                    c.Position <- Position.Row (t, this.ItemSize)
                    t <- t + this.ItemSize + this.Spacing
                    b <- t + this.ItemSize
            if b <> content_height then
                content_height <- b
                contentChangeEvent.Trigger content_height

        override this.Navigate() =
            if (!|"up").Tapped() then this.Previous()
            elif (!|"down").Tapped() then this.Next()
            elif (!|"select").Tapped() then this.SelectFocusedChild()

    [<Sealed>]
    type LeftToRight<'T when 'T :> Widget>(item_width: float32) =
        inherit Base<'T>(item_width)
        
        let mutable content_width = 0.0f
        let contentChangeEvent = Event<float32>()
        member this.ContentWidthChanged = contentChangeEvent.Publish

        override this.FlowContent children =
            let mutable l = 0.0f
            let mutable r = 0.0f
            for { Widget = c; Visible = visible } in children do
                if visible then
                    c.Position <- Position.Column (l, this.ItemSize)
                    l <- l + this.ItemSize + this.Spacing
                    r <- l + this.ItemSize
            if r <> content_width then
                content_width <- r
                contentChangeEvent.Trigger content_width
                    
        override this.Navigate() =
            if (!|"left").Tapped() then this.Previous()
            elif (!|"right").Tapped() then this.Next()
            elif (!|"select").Tapped() then this.SelectFocusedChild()

    [<Sealed>]
    type RightToLeft<'T when 'T :> Widget>(item_width: float32) =
        inherit Base<'T>(item_width)

        let mutable content_width = 0.0f
        let contentChangeEvent = Event<float32>()
        member this.ContentWidthChanged = contentChangeEvent.Publish

        override this.FlowContent children =
            let mutable r = 0.0f
            let mutable l = 0.0f
            for { Widget = c; Visible = visible } in children do
                if visible then
                    c.Position <- 
                        { Left = 1.0f %- (r + this.ItemSize); Top = Position.min; Right = 1.0f %- r; Bottom = Position.max }
                    r <- r + this.ItemSize + this.Spacing
                    l <- r + this.ItemSize
                if l <> content_width then
                    content_width <- l
                    contentChangeEvent.Trigger content_width
                    
        override this.Navigate() =
            if (!|"left").Tapped() then this.Next()
            elif (!|"right").Tapped() then this.Previous()
            elif (!|"select").Tapped() then this.SelectFocusedChild()

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

    member val Margin = 0.0f with get, set
    member this.ContentHeight with set(value) = contentHeight <- value; refresh <- true

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
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
                scrollby <- scrollby + selected_bounds.Bottom - this.Bounds.Bottom
            elif this.Bounds.Top > selected_bounds.Top then
                scrollby <- scrollby - this.Bounds.Top + selected_bounds.Top

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
            if before <> after then scroll_to_child_next_frame <- true
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

    member this.Scroll(amount: float32) =
        scroll_pos.Target <- Math.Max(0.0f, Math.Min(scroll_pos.Target + amount, contentHeight - this.Bounds.Height))

/// Container that provides navigation and selection using arrow keys + enter
/// Content is assumed to be positioned in a layout that fits the navigation
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
            
/// Container whose content can be swapped for other widgets
type SwapContainer() as this =
    inherit StaticWidget(NodeType.Switch (fun () -> this.Current))

    let mutable current : Widget = Unchecked.defaultof<_>
    let mutable justSwapped = false

    member this.Current
        with get() = current
        and set(v) =
            current <- v
            if this.Initialised then
                if not current.Initialised then current.Init this
                else assert(current.Parent = this)
            justSwapped <- true

    override this.Init(parent) =
        base.Init parent
        if isNull (current :> obj) then 
            Logging.Error("SwapContainer was not given child element before init")
        else current.Init this

    override this.Draw() =
        current.Draw()

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        let moved = if justSwapped then justSwapped <- false; true else moved
        current.Update(elapsedTime, moved)