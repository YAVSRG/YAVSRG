namespace Percyqaz.Flux.UI

open System.Linq
open System.Runtime.CompilerServices
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.Windowing

/// Container that provides navigation and selection using arrow keys + enter
/// Content is assumed to be positioned in a layout that fits the navigation
module NavigationContainer =

    [<AbstractClass>]
    type Base() as this =
        inherit StaticWidget(NodeType.Container(fun _ -> this.WhoShouldFocus |> Option.map (fun x -> x :> ISelection)))

        let children = ResizeArray<Widget>()
        let mutable last_selected = 0

        member val WrapNavigation = true with get, set

        member private this.WhoIsFocused: int option =
            Seq.tryFindIndex (fun (c: Widget) -> c.Focused) children

        member private this.WhoShouldFocus: Widget option =
            if children.Count = 0 then
                None
            else

                if last_selected >= children.Count then
                    last_selected <- 0

                Some children.[last_selected]

        override this.Focusable = if children.Count = 0 then false else base.Focusable

        member this.Previous() =
            match this.WhoIsFocused with
            | Some i ->
                let mutable index = (i + children.Count - 1) % children.Count

                while index <> i && not children.[index].Focusable do
                    index <- (index + children.Count - 1) % children.Count

                last_selected <- index
                children.[index].Focus false
            | None -> this.WhoShouldFocus.Value.Focus false

        member this.Next() =
            match this.WhoIsFocused with
            | Some i ->
                let mutable index = (i + 1) % children.Count

                while index <> i && not children.[index].Focusable do
                    index <- (index + 1) % children.Count

                last_selected <- index
                children.[index].Focus false
            | None -> this.WhoShouldFocus.Value.Focus false

        member this.CanPrevious() =
            if this.WrapNavigation then
                true
            else
                match this.WhoIsFocused with
                | Some i -> Seq.indexed children |> Seq.exists (fun (idx, c) -> idx < i && c.Focusable)
                | None -> true

        member this.CanNext() =
            if this.WrapNavigation then
                true
            else
                match this.WhoIsFocused with
                | Some i -> Seq.indexed children |> Seq.exists (fun (idx, c) -> idx > i && c.Focusable)
                | None -> true

        member this.SelectFocusedChild() =
            match this.WhoIsFocused with
            | Some i ->
                last_selected <- i
                children.[i].Select false
            | None -> ()

        abstract member Navigate: unit -> unit

        override this.Draw() =
            for i = children.Count - 1 downto 0 do
                children.[i].Draw()

        override this.Update(elapsed_ms, moved) =
            base.Update(elapsed_ms, moved)

            for c in children do
                c.Update(elapsed_ms, moved)

            if this.Focused && children.Count > 0 then
                this.Navigate()

        member this.Add(child: #Widget) = (this :> IContainer<Widget>).Add child
        member this.Remove(child: Widget) : bool = (this :> IContainer<Widget>).Remove child

        override this.Init(parent: Widget) =
            base.Init parent

            for c in children do
                c.Init this

        member this.Clear() =
            assert(GameThread.is_game_thread())
            children.Clear()

        static member (|+)(parent: #Base, child: Widget) =
            parent.Add child
            parent

        static member (|+)(parent: #Base, children: Widget seq) =
            Seq.iter parent.Add children
            parent

        static member (|*)(parent: #Base, child: Widget) = parent.Add child
        static member (|*)(parent: #Base, children: Widget seq) = Seq.iter parent.Add children

        interface IContainer<Widget> with

            member this.Add (child: Widget) : unit =
                assert(GameThread.is_game_thread())
                children.Add child

                if this.Initialised then
                    child.Init this

            member this.Remove (child: Widget) : bool =
                assert(GameThread.is_game_thread())
                children.Remove child

    [<Sealed>]
    type Column() =
        inherit Base()

        override this.Navigate() =
            if this.CanPrevious() && (%%"up").Pressed() then
                this.Previous()

            if this.CanNext() && (%%"down").Pressed() then
                this.Next()

            if (%%"select").Pressed() then
                this.SelectFocusedChild()

    [<Sealed>]
    type Row() =
        inherit Base()

        override this.Navigate() =
            if this.CanPrevious() && (%%"left").Pressed() then
                this.Previous()

            if this.CanNext() && (%%"right").Pressed() then
                this.Next()

            if (%%"select").Pressed() then
                this.SelectFocusedChild()

    type private GridSwitchItem = { Widget: Widget; X: int; Y: int }

    [<Sealed>]
    type Grid() as this =
        inherit StaticWidget(NodeType.Container(fun _ -> this.WhoShouldFocus))

        let mutable rows = 0
        let mutable columns = 0
        let mutable last_selected = 0
        let children = ResizeArray<GridSwitchItem>()

        member this.Clear() =
            children.Clear()
            rows <- 0
            columns <- 0

        member val Floating = false with get, set
        member val WrapNavigation = true with get, set

        member private this.WhoIsFocused: int option =
            Seq.tryFindIndex (fun (c: GridSwitchItem) -> c.Widget.Focused) children

        member private this.WhoShouldFocus: ISelection option =
            if children.Count = 0 then
                None
            else

                if last_selected >= children.Count then
                    last_selected <- 0

                Some children.[last_selected].Widget

        override this.Focusable = if children.Count = 0 then false else base.Focusable

        member private this.Up() =
            match this.WhoIsFocused with
            | Some i ->
                let c = children.[i]
                let mutable p = (c.Y - 1) %% rows

                let mutable found =
                    Seq.tryFindIndex
                        (fun (item: GridSwitchItem) -> item.X = c.X && item.Y = p && item.Widget.Focusable)
                        children

                while found.IsNone && p <> c.Y do
                    p <- (p - 1) %% rows

                    found <-
                        Seq.tryFindIndex
                            (fun (item: GridSwitchItem) -> item.X = c.X && item.Y = p && item.Widget.Focusable)
                            children

                match found with
                | Some i ->
                    last_selected <- i
                    children.[i].Widget.Focus false
                | None -> ()
            | None -> ()

        member private this.Down() =
            match this.WhoIsFocused with
            | Some i ->
                let c = children.[i]
                let mutable p = (c.Y + 1) %% rows

                let mutable found =
                    Seq.tryFindIndex
                        (fun (item: GridSwitchItem) -> item.X = c.X && item.Y = p && item.Widget.Focusable)
                        children

                while found.IsNone && p <> c.Y do
                    p <- (p + 1) %% rows

                    found <-
                        Seq.tryFindIndex
                            (fun (item: GridSwitchItem) -> item.X = c.X && item.Y = p && item.Widget.Focusable)
                            children

                match found with
                | Some i ->
                    last_selected <- i
                    children.[i].Widget.Focus false
                | None -> ()
            | None -> ()

        member private this.Left() =
            match this.WhoIsFocused with
            | Some i ->
                let c = children.[i]
                let mutable p = (c.X - 1) %% columns

                let mutable found =
                    Seq.tryFindIndex
                        (fun (item: GridSwitchItem) -> item.X = p && item.Y = c.Y && item.Widget.Focusable)
                        children

                while found.IsNone && p <> c.X do
                    p <- (p - 1) %% columns

                    found <-
                        Seq.tryFindIndex
                            (fun (item: GridSwitchItem) -> item.X = p && item.Y = c.Y && item.Widget.Focusable)
                            children

                match found with
                | Some i ->
                    last_selected <- i
                    children.[i].Widget.Focus false
                | None -> ()
            | None -> ()

        member private this.Right() =
            match this.WhoIsFocused with
            | Some i ->
                let c = children.[i]
                let mutable p = (c.X + 1) %% columns

                let mutable found =
                    Seq.tryFindIndex
                        (fun (item: GridSwitchItem) -> item.X = p && item.Y = c.Y && item.Widget.Focusable)
                        children

                while found.IsNone && p <> c.X do
                    p <- (p + 1) %% columns

                    found <-
                        Seq.tryFindIndex
                            (fun (item: GridSwitchItem) -> item.X = p && item.Y = c.Y && item.Widget.Focusable)
                            children

                match found with
                | Some i ->
                    last_selected <- i
                    children.[i].Widget.Focus false
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

                    children.Any(fun (item: GridSwitchItem) ->
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

                    children.Any(fun (item: GridSwitchItem) ->
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

                    children.Any(fun (item: GridSwitchItem) ->
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

                    children.Any(fun (item: GridSwitchItem) ->
                        item.X > c.X && item.Y = c.Y && item.Widget.Focusable
                    )
                | None -> false

        override this.Update(elapsed_ms, moved) =
            base.Update(elapsed_ms, moved)

            for { Widget = c } in children do
                if moved || this.Floating || c.VisibleBounds.Visible then
                    c.Update(elapsed_ms, moved)

            if this.Focused then

                if this.CanUp() && (%%"up").Pressed() then
                    this.Up()

                if this.CanDown() && (%%"down").Pressed() then
                    this.Down()

                if this.CanLeft() && (%%"left").Pressed() then
                    this.Left()

                if this.CanRight() && (%%"right").Pressed() then
                    this.Right()

                if (%%"select").Pressed() then
                    match this.WhoIsFocused with
                    | Some i ->
                        last_selected <- i
                        children.[i].Widget.Select false
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

[<Extension>]
type NavigationContainerExtensions =

    [<Extension>]
    static member WrapNavigation(this: #NavigationContainer.Base, wrap: bool) : #NavigationContainer.Base =
        this.WrapNavigation <- wrap
        this

    [<Extension>]
    static member WrapNavigation(this: NavigationContainer.Grid, wrap: bool) : NavigationContainer.Grid =
        this.WrapNavigation <- wrap
        this

    [<Extension>]
    static member Floating(this: NavigationContainer.Grid) : NavigationContainer.Grid =
        this.Floating <- true
        this