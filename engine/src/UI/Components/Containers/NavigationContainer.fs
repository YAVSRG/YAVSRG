namespace Percyqaz.Flux.UI

open System.Linq
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.Utils

/// Container that provides navigation and selection using arrow keys + enter
/// Content is assumed to be positioned in a layout that fits the navigation
module NavigationContainer =

    [<AbstractClass>]
    type Base<'T when 'T :> Widget>() as this =
        inherit StaticWidget(NodeType.Switch(fun _ -> this.WhoShouldFocus))

        let children = ResizeArray<'T>()
        let mutable last_selected = 0
        
        member val WrapNavigation = true with get, set

        override this.Focus() =
            if children.Count > 0 then
                base.Focus()

        override this.Select() =
            if children.Count > 0 then
                base.Select()

        member private this.WhoIsFocused: int option =
            Seq.tryFindIndex (fun (c: 'T) -> c.Focused) children

        member private this.WhoShouldFocus : ISelection =
            if children.Count = 0 then this else

            if last_selected >= children.Count then
                last_selected <- 0

            children.[last_selected]

        override this.Focusable = if children.Count = 0 then false else base.Focusable

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

        member this.CanPrevious() =
            if this.WrapNavigation then
                true
            else
                match this.WhoIsFocused with
                | Some i -> Seq.indexed children |> Seq.exists (fun (idx, c) -> idx < i && c.Focusable)
                | None -> false
        
        member this.CanNext() =
            if this.WrapNavigation then
                true
            else
                match this.WhoIsFocused with
                | Some i -> Seq.indexed children |> Seq.exists (fun (idx, c) -> idx > i && c.Focusable)
                | None -> false

        member this.SelectFocusedChild() =
            match this.WhoIsFocused with
            | Some i ->
                last_selected <- i
                children.[i].Select()
            | None -> ()

        abstract member Navigate: unit -> unit

        override this.Draw() =
            for c in children do
                c.Draw()

        override this.Update(elapsed_ms, moved) =
            base.Update(elapsed_ms, moved)

            for i = children.Count - 1 downto 0 do
                children.[i].Update(elapsed_ms, moved)

            if this.Focused then
                this.Navigate()

        member this.Add(child: 'T) =
            require_ui_thread ()
            children.Add child

            if this.Initialised then
                child.Init this

        override this.Init(parent: Widget) =
            base.Init parent

            for c in children do
                c.Init this

        member this.Clear() =
            require_ui_thread ()
            children.Clear()

        static member (|+)(parent: #Base<'T>, child: 'T) =
            parent.Add child
            parent
        static member (|+)(parent: #Base<'T>, children: 'T seq) =
            Seq.iter parent.Add children
            parent

        static member (|*)(parent: #Base<'T>, child: 'T) = parent.Add child
        static member (|*)(parent: #Base<'T>, children: 'T seq) = Seq.iter parent.Add children

    [<Sealed>]
    type Column<'T when 'T :> Widget>() =
        inherit Base<'T>()

        override this.Navigate() =
            if this.CanPrevious() && (%%"up").Tapped() then
                this.Previous()

            if this.CanNext() && (%%"down").Tapped() then
                this.Next()

            if (%%"select").Tapped() then
                this.SelectFocusedChild()

    [<Sealed>]
    type Row<'T when 'T :> Widget>() =
        inherit Base<'T>()

        override this.Navigate() =
            if this.CanPrevious() && (%%"left").Tapped() then
                this.Previous()

            if this.CanNext() && (%%"right").Tapped() then
                this.Next()

            if (%%"select").Tapped() then
                this.SelectFocusedChild()

    type private GridSwitchItem<'T when 'T :> Widget> = { Widget: 'T; X: int; Y: int }

    [<Sealed>]
    type Grid<'T when 'T :> Widget>() as this =
        inherit StaticWidget(NodeType.Switch(fun _ -> this.WhoShouldFocus))

        let mutable rows = 0
        let mutable columns = 0
        let mutable last_selected = 0
        let children = ResizeArray<GridSwitchItem<'T>>()

        override this.Focus() =
            if children.Count > 0 then
                base.Focus()

        override this.Select() =
            if children.Count > 0 then
                base.Select()

        member this.Clear() =
            children.Clear()
            rows <- 0
            columns <- 0

        member val Floating = false with get, set
        member val WrapNavigation = true with get, set

        member private this.WhoIsFocused: int option =
            Seq.tryFindIndex (fun (c: GridSwitchItem<'T>) -> c.Widget.Focused) children

        member private this.WhoShouldFocus : ISelection =
            if children.Count = 0 then this else

            if last_selected >= children.Count then
                last_selected <- 0

            children.[last_selected].Widget

        override this.Focusable = if children.Count = 0 then false else base.Focusable

        member private this.Up() =
            match this.WhoIsFocused with
            | Some i ->
                let c = children.[i]
                let mutable p = (c.Y - 1) %% rows

                let mutable found =
                    Seq.tryFindIndex
                        (fun (item: GridSwitchItem<'T>) -> item.X = c.X && item.Y = p && item.Widget.Focusable)
                        children

                while found.IsNone && p <> c.Y do
                    p <- (p - 1) %% rows

                    found <-
                        Seq.tryFindIndex
                            (fun (item: GridSwitchItem<'T>) -> item.X = c.X && item.Y = p && item.Widget.Focusable)
                            children

                match found with
                | Some i ->
                    last_selected <- i
                    children.[i].Widget.Focus()
                | None -> ()
            | None -> ()

        member private this.Down() =
            match this.WhoIsFocused with
            | Some i ->
                let c = children.[i]
                let mutable p = (c.Y + 1) %% rows

                let mutable found =
                    Seq.tryFindIndex
                        (fun (item: GridSwitchItem<'T>) -> item.X = c.X && item.Y = p && item.Widget.Focusable)
                        children

                while found.IsNone && p <> c.Y do
                    p <- (p + 1) %% rows

                    found <-
                        Seq.tryFindIndex
                            (fun (item: GridSwitchItem<'T>) -> item.X = c.X && item.Y = p && item.Widget.Focusable)
                            children

                match found with
                | Some i ->
                    last_selected <- i
                    children.[i].Widget.Focus()
                | None -> ()
            | None -> ()

        member private this.Left() =
            match this.WhoIsFocused with
            | Some i ->
                let c = children.[i]
                let mutable p = (c.X - 1) %% columns

                let mutable found =
                    Seq.tryFindIndex
                        (fun (item: GridSwitchItem<'T>) -> item.X = p && item.Y = c.Y && item.Widget.Focusable)
                        children

                while found.IsNone && p <> c.X do
                    p <- (p - 1) %% columns

                    found <-
                        Seq.tryFindIndex
                            (fun (item: GridSwitchItem<'T>) -> item.X = p && item.Y = c.Y && item.Widget.Focusable)
                            children

                match found with
                | Some i ->
                    last_selected <- i
                    children.[i].Widget.Focus()
                | None -> ()
            | None -> ()

        member private this.Right() =
            match this.WhoIsFocused with
            | Some i ->
                let c = children.[i]
                let mutable p = (c.X + 1) %% columns

                let mutable found =
                    Seq.tryFindIndex
                        (fun (item: GridSwitchItem<'T>) -> item.X = p && item.Y = c.Y && item.Widget.Focusable)
                        children

                while found.IsNone && p <> c.X do
                    p <- (p + 1) %% columns

                    found <-
                        Seq.tryFindIndex
                            (fun (item: GridSwitchItem<'T>) -> item.X = p && item.Y = c.Y && item.Widget.Focusable)
                            children

                match found with
                | Some i ->
                    last_selected <- i
                    children.[i].Widget.Focus()
                | None -> ()
            | None -> ()

        override this.Init(parent: Widget) =
            base.Init parent

            for c in children do
                c.Widget.Init this

        member private this.CanUp() =
            if this.WrapNavigation then
                true
            else

                match this.WhoIsFocused with
                | Some i ->
                    let c = children.[i]

                    children.Any(fun (item: GridSwitchItem<'T>) ->
                        item.X = c.X && item.Y < c.Y && item.Widget.Focusable
                    )
                | None -> false

        member private this.CanDown() =
            if this.WrapNavigation then
                true
            else

                match this.WhoIsFocused with
                | Some i ->
                    let c = children.[i]

                    children.Any(fun (item: GridSwitchItem<'T>) ->
                        item.X = c.X && item.Y > c.Y && item.Widget.Focusable
                    )
                | None -> false

        member private this.CanLeft() =
            if this.WrapNavigation then
                true
            else

                match this.WhoIsFocused with
                | Some i ->
                    let c = children.[i]

                    children.Any(fun (item: GridSwitchItem<'T>) ->
                        item.X < c.X && item.Y = c.Y && item.Widget.Focusable
                    )
                | None -> false

        member private this.CanRight() =
            if this.WrapNavigation then
                true
            else

                match this.WhoIsFocused with
                | Some i ->
                    let c = children.[i]

                    children.Any(fun (item: GridSwitchItem<'T>) ->
                        item.X > c.X && item.Y = c.Y && item.Widget.Focusable
                    )
                | None -> false

        override this.Update(elapsed_ms, moved) =
            base.Update(elapsed_ms, moved)

            for { Widget = c } in children do
                if moved || this.Floating || c.VisibleBounds.Visible then
                    c.Update(elapsed_ms, moved)

            if this.Focused then

                if this.CanUp() && (%%"up").Tapped() then
                    this.Up()

                if this.CanDown() && (%%"down").Tapped() then
                    this.Down()

                if this.CanLeft() && (%%"left").Tapped() then
                    this.Left()

                if this.CanRight() && (%%"right").Tapped() then
                    this.Right()

                if (%%"select").Tapped() then
                    match this.WhoIsFocused with
                    | Some i ->
                        last_selected <- i
                        children.[i].Widget.Select()
                    | None -> ()

        override this.Draw() =
            for { Widget = c } in children do
                if this.Floating || c.VisibleBounds.Visible then
                    c.Draw()

        member this.Add(child: 'T, x: int, y: int) =
            if x < 0 || y < 0 then
                failwith "X and Y can't be negative"

            rows <- max rows (y + 1)
            columns <- max columns (x + 1)
            children.Add { Widget = child; X = x; Y = y }

            if this.Initialised then
                child.Init this
