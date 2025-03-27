namespace Percyqaz.Flux.UI

open System
open System.Runtime.CompilerServices
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.Windowing

/// Container that automatically positions its contents stacked in Vertical/Horizontal arrangements
module FlowContainer =

    type FlowItem<'T when 'T :> Widget> = { Widget: 'T; mutable Visible: bool } // todo: store size in this object, get it from the widget OR pass in when adding

    [<AbstractClass>]
    type Base<'T when 'T :> Widget>(item_size: float32) as this =
        inherit StaticWidget(NodeType.Container(fun _ -> this.WhoShouldFocus |> Option.map (fun x -> x :> ISelection)))

        let mutable size_change = ignore
        let mutable filter: 'T -> bool = K true
        let mutable sort: ('T -> 'T -> int) option = None
        let mutable spacing = 0.0f
        let mutable item_size = item_size
        let mutable refresh = true
        let mutable last_selected = 0
        let children = ResizeArray<FlowItem<'T>>()

        /// Total children in this container, visible or not
        member this.Count = children.Count

        member private this.WhoIsFocused: int option =
            Seq.tryFindIndex (fun c -> c.Widget.Focused) children

        member private this.WhoShouldFocus: Widget option =
            if children.Count = 0 then
                None
            else

                if last_selected >= children.Count then
                    last_selected <- 0

                Some children.[last_selected].Widget

        override this.Focusable = if children.Count = 0 then false else base.Focusable

        member this.Previous() =
            match this.WhoIsFocused with
            | Some i ->
                let mutable index = (i + children.Count - 1) % children.Count

                while index <> i
                      && (not children.[index].Visible || not children.[index].Widget.Focusable) do
                    index <- (index + children.Count - 1) % children.Count

                last_selected <- index
                children.[index].Widget.Focus false
            | None -> this.WhoShouldFocus.Value.Focus false

        member this.Next() =
            match this.WhoIsFocused with
            | Some i ->
                let mutable index = (i + 1) % children.Count

                while index <> i
                      && (not children.[index].Visible || not children.[index].Widget.Focusable) do
                    index <- (index + 1) % children.Count

                last_selected <- index
                children.[index].Widget.Focus false
            | None -> this.WhoShouldFocus.Value.Focus false

        member this.SelectFocusedChild() =
            match this.WhoIsFocused with
            | Some i ->
                last_selected <- i
                children.[i].Widget.Select false
            | None -> ()

        override this.OnUnfocus(by_mouse: bool) =
            match this.WhoIsFocused with
            | Some i -> last_selected <- i
            | None -> ()

            base.OnUnfocus by_mouse

        abstract member Navigate: unit -> unit

        member this.Filter
            with set value =
                filter <- value

                for c in children do
                    c.Visible <- filter c.Widget

                refresh <- true
                size_change()

        member this.Sort
            with set (comp: 'T -> 'T -> int) =
                sort <- Some comp
                children.Sort(fun { Widget = a } { Widget = b } -> comp a b)
                refresh <- true

        member this.Spacing
            with get () = spacing
            and set (value) =
                spacing <- value
                refresh <- true
                size_change()

        member this.ItemSize
            with get () = item_size
            and set (value) =
                item_size <- value
                refresh <- true
                size_change()

        member val Floating = false with get, set
        member val EnableNavigation = true with get, set

        member this.ContentSize =
            let count = children |> Seq.where (_.Visible) |> Seq.length
            float32 count * item_size + (float32 count - 1.0f) * spacing

        member this.Clear() =
            assert(GameThread.is_game_thread())
            children.Clear()
            refresh <- true
            size_change()

        override this.Draw() =
            for i = children.Count - 1 downto 0 do
                let { Widget = c; Visible = visible } = children.[i]

                if visible && (this.Floating || c.VisibleBounds.Visible) then
                    c.Draw()

        override this.Update(elapsed_ms, moved) =
            base.Update(elapsed_ms, moved || refresh)

            let moved =
                if refresh then
                    refresh <- false
                    this.FlowContent children
                    true
                else
                    moved

            for child in children do
                if child.Visible && (moved || this.Floating || child.Widget.VisibleBounds.Visible) then
                    child.Widget.Update(elapsed_ms, moved)

            if this.EnableNavigation && this.Focused && children.Count > 0 then
                this.Navigate()

        abstract member FlowContent: ResizeArray<FlowItem<'T>> -> unit

        member this.Add(child: 'T) = (this :> IContainer<'T>).Add child
        member this.Remove(child: 'T) : bool = (this :> IContainer<'T>).Remove child

        override this.Init(parent: Widget) =
            base.Init parent
            this.FlowContent children

            for { Widget = c } in children do
                c.Init this

        member this.Iter(f: 'T -> unit) =
            assert(GameThread.is_game_thread())

            for { Widget = c } in children do
                f c

        static member (|+)(parent: #Base<'T>, child: 'T) =
            parent.Add child
            parent

        static member (|+)(parent: #Base<'T>, children: 'T seq) =
            Seq.iter parent.Add children
            parent

        static member (|*)(parent: #Base<'T>, child: 'T) = parent.Add child
        static member (|*)(parent: #Base<'T>, children: 'T seq) = Seq.iter parent.Add children

        interface IContainer<'T> with

            member this.Add(child: 'T) : unit =
                assert(GameThread.is_game_thread())

                children.Add
                    {
                        Widget = child
                        Visible = filter child
                    }

                match sort with
                | Some comp -> children.Sort(fun { Widget = a } { Widget = b } -> comp a b)
                | None -> ()

                if this.Initialised then
                    child.Init this
                    refresh <- true

                size_change()

            member this.Remove(child: 'T) : bool =
                assert(GameThread.is_game_thread())

                match Seq.tryFind (fun { Widget = c } -> Object.ReferenceEquals(c, child)) children with
                | Some x ->
                    children.Remove x |> ignore
                    refresh <- true
                    size_change()
                    true
                | None -> false

        interface IResize with
            member this.OnSizeChanged
                with set v = size_change <- v

    [<Sealed>]
    type Vertical<'T when 'T :> Widget>(item_height: float32) =
        inherit Base<'T>(item_height)

        override this.FlowContent children =
            let mutable t = 0.0f

            for { Widget = c; Visible = visible } in children do
                if visible then
                    c.Position <- Position.SliceT(t, this.ItemSize)
                    t <- t + this.ItemSize + this.Spacing

        override this.Navigate() =
            if (%%"up").Pressed() then
                this.Previous()

            if (%%"down").Pressed() then
                this.Next()

            if Selection.get_focused_element() <> Some this && (%%"select").Pressed() then
                this.SelectFocusedChild()

        interface IHeight with
            member this.Height = this.ContentSize

    [<Sealed>]
    type LeftToRight<'T when 'T :> Widget>(item_width: float32) =
        inherit Base<'T>(item_width)

        override this.FlowContent children =
            let mutable l = 0.0f

            for { Widget = c; Visible = visible } in children do
                if visible then
                    c.Position <- Position.SliceL(l, this.ItemSize)
                    l <- l + this.ItemSize + this.Spacing

        override this.Navigate() =
            if (%%"left").Pressed() then
                this.Previous()

            if (%%"right").Pressed() then
                this.Next()

            if Selection.get_focused_element() <> Some this && (%%"select").Pressed() then
                this.SelectFocusedChild()

        interface IWidth with
            member this.Width = this.ContentSize

    [<Sealed>]
    type RightToLeft<'T when 'T :> Widget>(item_width: float32) =
        inherit Base<'T>(item_width)

        override this.FlowContent children =
            let mutable r = 0.0f

            for { Widget = c; Visible = visible } in children do
                if visible then
                    c.Position <-
                        {
                            Left = 1.0f %- (r + this.ItemSize)
                            Top = Position.MIN
                            Right = 1.0f %- r
                            Bottom = Position.MAX
                        }
                    r <- r + this.ItemSize + this.Spacing

        override this.Navigate() =
            if (%%"left").Pressed() then
                this.Next()

            if (%%"right").Pressed() then
                this.Previous()

            if Selection.get_focused_element() <> Some this && (%%"select").Pressed() then
                this.SelectFocusedChild()

        interface IWidth with
            member this.Width = this.ContentSize

[<Extension>]
type FlowContainerExtensions =

    [<Extension>]
    static member Spacing(container: #FlowContainer.Base<'T>, spacing: float32) : #FlowContainer.Base<'T> =
        container.Spacing <- spacing
        container

    [<Extension>]
    static member ItemSize(container: #FlowContainer.Base<'T>, item_size: float32) : #FlowContainer.Base<'T> =
        container.ItemSize <- item_size
        container

    [<Extension>]
    static member Filter(container: #FlowContainer.Base<'T>, filter: 'T -> bool) : unit =
        container.Filter <- filter

    [<Extension>]
    static member WithFilter(container: #FlowContainer.Base<'T>, filter: 'T -> bool) : #FlowContainer.Base<'T> =
        container.Filter <- filter
        container

    [<Extension>]
    static member Sort(container: #FlowContainer.Base<'T>, cmp: 'T -> 'T -> int) : unit =
        container.Sort <- cmp

    [<Extension>]
    static member WithSort(container: #FlowContainer.Base<'T>, cmp: 'T -> 'T -> int) : #FlowContainer.Base<'T> =
        container.Sort <- cmp
        container

    [<Extension>]
    static member Floating(container: #FlowContainer.Base<'T>) : #FlowContainer.Base<'T> =
        container.Floating <- true
        container

    [<Extension>]
    static member DisableNavigation(container: #FlowContainer.Base<'T>) : #FlowContainer.Base<'T> =
        container.EnableNavigation <- false
        container