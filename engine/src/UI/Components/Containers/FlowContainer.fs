namespace Percyqaz.Flux.UI

open System
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.Utils

/// Container that automatically positions its contents stacked in Vertical/Horizontal arrangements
module FlowContainer =

    type FlowItem<'T when 'T :> Widget> = { Widget: 'T; mutable Visible: bool }

    [<AbstractClass>]
    type Base<'T when 'T :> Widget>(item_size: float32) as this =
        inherit StaticWidget(NodeType.Switch(fun _ -> this.WhoShouldFocus))

        let mutable filter: 'T -> bool = K true
        let mutable sort: ('T -> 'T -> int) option = None
        let mutable spacing = 0.0f
        let mutable item_size = item_size
        let mutable refresh = true
        let mutable last_selected = 0
        let children = ResizeArray<FlowItem<'T>>()

        /// Total children in this container, visible or not
        member this.Count = children.Count

        override this.Focus() =
            if children.Count > 0 then
                base.Focus()

        override this.Select() =
            if children.Count > 0 then
                base.Select()

        member private this.WhoIsFocused: int option =
            Seq.tryFindIndex (fun c -> c.Widget.Focused) children

        member private this.WhoShouldFocus =
            if children.Count = 0 then
                failwithf "Tried to focus this %O with no children" this

            if last_selected >= children.Count then
                last_selected <- 0

            children.[last_selected].Widget

        override this.Focusable = if children.Count = 0 then false else base.Focusable

        member this.Previous() =
            match this.WhoIsFocused with
            | Some i ->
                let mutable index = (i + children.Count - 1) % children.Count

                while index <> i
                      && (not children.[index].Visible || not children.[index].Widget.Focusable) do
                    index <- (index + children.Count - 1) % children.Count

                last_selected <- index
                children.[index].Widget.Focus()
            | None -> ()

        member this.Next() =
            match this.WhoIsFocused with
            | Some i ->
                let mutable index = (i + 1) % children.Count

                while index <> i
                      && (not children.[index].Visible || not children.[index].Widget.Focusable) do
                    index <- (index + 1) % children.Count

                last_selected <- index
                children.[index].Widget.Focus()
            | None -> ()

        member this.SelectFocusedChild() =
            match this.WhoIsFocused with
            | Some i ->
                last_selected <- i
                children.[i].Widget.Select()
            | None -> ()

        override this.OnUnfocus() =
            match this.WhoIsFocused with
            | Some i -> last_selected <- i
            | None -> ()

            base.OnUnfocus()

        abstract member Navigate: unit -> unit

        member this.Filter
            with set value =
                filter <- value

                for c in children do
                    c.Visible <- filter c.Widget

                refresh <- true

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

        member this.ItemSize
            with get () = item_size
            and set (value) =
                item_size <- value
                refresh <- true

        member val Floating = false with get, set
        member val AllowNavigation = true with get, set

        member this.Clear() =
            require_ui_thread ()
            children.Clear()

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

            for { Widget = c; Visible = visible } in children do
                if visible && (moved || this.Floating || c.VisibleBounds.Visible) then
                    c.Update(elapsed_ms, moved)

            if this.AllowNavigation && this.Focused then
                this.Navigate()

        abstract member FlowContent: ResizeArray<FlowItem<'T>> -> unit

        member this.Add(child: 'T) =
            require_ui_thread ()

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

        member this.Remove(child: 'T) =
            require_ui_thread ()

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
            require_ui_thread ()

            for { Widget = c } in children do
                f c

        static member (|+)(parent: #Base<'T>, child: 'T) =
            parent.Add child
            parent

        static member (|*)(parent: #Base<'T>, child: 'T) = parent.Add child

    [<Sealed>]
    type Vertical<'T when 'T :> Widget>(item_height: float32) =
        inherit Base<'T>(item_height)

        let mutable content_height = 0.0f
        let content_change_ev = Event<float32>()
        member this.ContentHeightChanged = content_change_ev.Publish

        override this.FlowContent children =
            let mutable t = 0.0f
            let mutable b = 0.0f

            for { Widget = c; Visible = visible } in children do
                if visible then
                    c.Position <- Position.Row(t, this.ItemSize)
                    t <- t + this.ItemSize + this.Spacing
                    b <- t + this.ItemSize

            if b <> content_height then
                content_height <- b
                content_change_ev.Trigger content_height

        override this.Navigate() =
            if (%%"up").Tapped() then
                this.Previous()

            if (%%"down").Tapped() then
                this.Next()

            if (%%"select").Tapped() then
                this.SelectFocusedChild()

    [<Sealed>]
    type LeftToRight<'T when 'T :> Widget>(item_width: float32) =
        inherit Base<'T>(item_width)

        let mutable content_width = 0.0f
        let content_change_ev = Event<float32>()
        member this.ContentWidthChanged = content_change_ev.Publish

        override this.FlowContent children =
            let mutable l = 0.0f
            let mutable r = 0.0f

            for { Widget = c; Visible = visible } in children do
                if visible then
                    c.Position <- Position.Column(l, this.ItemSize)
                    l <- l + this.ItemSize + this.Spacing
                    r <- l + this.ItemSize

            if r <> content_width then
                content_width <- r
                content_change_ev.Trigger content_width

        override this.Navigate() =
            if (%%"left").Tapped() then
                this.Previous()

            if (%%"right").Tapped() then
                this.Next()

            if (%%"select").Tapped() then
                this.SelectFocusedChild()

    [<Sealed>]
    type RightToLeft<'T when 'T :> Widget>(item_width: float32) =
        inherit Base<'T>(item_width)

        let mutable content_width = 0.0f
        let content_change_ev = Event<float32>()
        member this.ContentWidthChanged = content_change_ev.Publish

        override this.FlowContent children =
            let mutable r = 0.0f
            let mutable l = 0.0f

            for { Widget = c; Visible = visible } in children do
                if visible then
                    c.Position <-
                        {
                            Left = 1.0f %- (r + this.ItemSize)
                            Top = Position.min
                            Right = 1.0f %- r
                            Bottom = Position.max
                        }

                    r <- r + this.ItemSize + this.Spacing
                    l <- r + this.ItemSize

                if l <> content_width then
                    content_width <- l
                    content_change_ev.Trigger content_width

        override this.Navigate() =
            if (%%"left").Tapped() then
                this.Next()

            if (%%"right").Tapped() then
                this.Previous()

            if (%%"select").Tapped() then
                this.SelectFocusedChild()
