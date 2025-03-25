namespace Percyqaz.Flux.UI

open System.Linq
open System.Runtime.CompilerServices
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.Windowing

type private GridFlowItem<'T when 'T :> Widget> =
    {
        Widget: 'T
        mutable Visible: bool
        mutable X: int
        mutable Y: int
    }

/// Container that automatically positions its contents packed in a grid arrangement
type GridFlowContainer<'T when 'T :> Widget>(row_height, columns: int) as this =
    inherit StaticWidget(NodeType.Container(fun _ -> this.WhoShouldFocus |> Option.map (fun x -> x :> ISelection)))

    let mutable spacing = 0.0f, 0.0f
    let mutable filter: 'T -> bool = K true
    let mutable refresh = false
    let mutable last_selected = 0
    let children = ResizeArray<GridFlowItem<'T>>()

    let mutable size_change = ignore
    let mutable content_height = 0.0f

    member this.Clear() =
        assert(GameThread.is_game_thread())
        children.Clear()

    member val Floating = false with get, set
    member val WrapNavigation = true with get, set

    member this.Filter
        with set value =
            filter <- value

            for c in children do
                c.Visible <- filter c.Widget

            refresh <- true

    member this.Spacing
        with get () = spacing
        and set (value) =
            spacing <- value
            refresh <- true

    member private this.WhoIsFocused: int option =
        Seq.tryFindIndex (fun (c: GridFlowItem<'T>) -> c.Widget.Focused) children

    member private this.WhoShouldFocus: Widget option =
        if children.Count = 0 then
            None
        else

            if last_selected >= children.Count then
                last_selected <- 0

            Some children.[last_selected].Widget

    override this.Focusable = if children.Count = 0 then false else base.Focusable

    member this.PackContent() =
        let spacing_x, spacing_y = this.Spacing

        let width =
            (this.Bounds.Width - (float32 columns - 1.0f) * spacing_x) / float32 columns

        let mutable x = 0
        let mutable y = 0
        let mutable height = 0.0f

        for c in children do
            if c.Visible then
                c.Widget.Position <-
                    Position.Box(
                        0.0f,
                        0.0f,
                        float32 x * (width + spacing_x),
                        float32 y * (row_height + spacing_y),
                        width,
                        row_height
                    )

                c.X <- x
                c.Y <- y
                height <- float32 y * (row_height + spacing_y) + row_height
                x <- x + 1

                if x = columns then
                    x <- 0
                    y <- y + 1

        if height <> content_height then
            content_height <- height
            size_change ()

    member private this.Up() =
        match this.WhoIsFocused with
        | Some i ->
            let c = children.[i]
            let rows = (children.Count + columns - 1) / columns
            let mutable p = (c.Y - 1) %% rows

            let mutable found =
                Seq.tryFindIndex
                    (fun (item: GridFlowItem<'T>) ->
                        item.X = c.X && item.Y = p && item.Widget.Focusable && item.Visible
                    )
                    children

            while found.IsNone && p <> c.Y do
                p <- (p - 1) %% rows

                found <-
                    Seq.tryFindIndex
                        (fun (item: GridFlowItem<'T>) ->
                            item.X = c.X && item.Y = p && item.Widget.Focusable && item.Visible
                        )
                        children

            match found with
            | Some i ->
                last_selected <- i
                children.[i].Widget.Focus false
            | None -> ()
        | None -> this.WhoShouldFocus.Value.Focus false

    member private this.Down() =
        match this.WhoIsFocused with
        | Some i ->
            let c = children.[i]
            let rows = (children.Count + columns - 1) / columns
            let mutable p = (c.Y + 1) %% rows

            let mutable found =
                Seq.tryFindIndex
                    (fun (item: GridFlowItem<'T>) ->
                        item.X = c.X && item.Y = p && item.Widget.Focusable && item.Visible
                    )
                    children

            while found.IsNone && p <> c.Y do
                p <- (p + 1) %% rows

                found <-
                    Seq.tryFindIndex
                        (fun (item: GridFlowItem<'T>) ->
                            item.X = c.X && item.Y = p && item.Widget.Focusable && item.Visible
                        )
                        children

            match found with
            | Some i ->
                last_selected <- i
                children.[i].Widget.Focus false
            | None -> ()
        | None -> this.WhoShouldFocus.Value.Focus false

    member private this.Left() =
        match this.WhoIsFocused with
        | Some i ->
            let c = children.[i]
            let mutable p = (c.X - 1) %% columns

            let mutable found =
                Seq.tryFindIndex
                    (fun (item: GridFlowItem<'T>) ->
                        item.X = p && item.Y = c.Y && item.Widget.Focusable && item.Visible
                    )
                    children

            while found.IsNone && p <> c.X do
                p <- (p - 1) %% columns

                found <-
                    Seq.tryFindIndex
                        (fun (item: GridFlowItem<'T>) ->
                            item.X = p && item.Y = c.Y && item.Widget.Focusable && item.Visible
                        )
                        children

            match found with
            | Some i ->
                last_selected <- i
                children.[i].Widget.Focus false
            | None -> ()
        | None -> this.WhoShouldFocus.Value.Focus false

    member private this.Right() =
        match this.WhoIsFocused with
        | Some i ->
            let c = children.[i]
            let mutable p = (c.X + 1) %% columns

            let mutable found =
                Seq.tryFindIndex
                    (fun (item: GridFlowItem<'T>) ->
                        item.X = p && item.Y = c.Y && item.Widget.Focusable && item.Visible
                    )
                    children

            while found.IsNone && p <> c.X do
                p <- (p + 1) %% columns

                found <-
                    Seq.tryFindIndex
                        (fun (item: GridFlowItem<'T>) ->
                            item.X = p && item.Y = c.Y && item.Widget.Focusable && item.Visible
                        )
                        children

            match found with
            | Some i ->
                last_selected <- i
                children.[i].Widget.Focus false
            | None -> ()
        | None -> this.WhoShouldFocus.Value.Focus false

    override this.Init(parent: Widget) =
        base.Init parent
        this.PackContent()

        for c in children do
            c.Widget.Init this

    member private this.CanUp() =
        if this.WrapNavigation then
            true
        else

            match this.WhoIsFocused with
            | Some i ->
                let c = children.[i]

                children.Any(fun (item: GridFlowItem<'T>) ->
                    item.X = c.X && item.Y < c.Y && item.Widget.Focusable && item.Visible
                )
            | None -> true

    member private this.CanDown() =
        if this.WrapNavigation then
            true
        else

            match this.WhoIsFocused with
            | Some i ->
                let c = children.[i]

                children.Any(fun (item: GridFlowItem<'T>) ->
                    item.X = c.X && item.Y > c.Y && item.Widget.Focusable && item.Visible
                )
            | None -> true

    member private this.CanLeft() =
        if this.WrapNavigation then
            true
        else

            match this.WhoIsFocused with
            | Some i ->
                let c = children.[i]

                children.Any(fun (item: GridFlowItem<'T>) ->
                    item.X < c.X && item.Y = c.Y && item.Widget.Focusable && item.Visible
                )
            | None -> true

    member private this.CanRight() =
        if this.WrapNavigation then
            true
        else

            match this.WhoIsFocused with
            | Some i ->
                let c = children.[i]

                children.Any(fun (item: GridFlowItem<'T>) ->
                    item.X > c.X && item.Y = c.Y && item.Widget.Focusable && item.Visible
                )
            | None -> true

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved || refresh)

        let moved =
            if moved || refresh then
                refresh <- false
                this.PackContent()
                true
            else
                false

        for { Widget = c; Visible = visible } in children do
            if visible && (moved || this.Floating || c.VisibleBounds.Visible) then
                c.Update(elapsed_ms, moved)

        if this.Focused && children.Count > 0 then

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
        for { Widget = c; Visible = visible } in children do
            if visible && (this.Floating || c.VisibleBounds.Visible) then
                c.Draw()

    member this.Add(child: 'T) : unit =
        assert(GameThread.is_game_thread())

        children.Add
            {
                Widget = child
                Visible = filter child
                X = -1
                Y = -1
            }

        if this.Initialised then
            child.Init this
            refresh <- true

    static member (|+)(parent: #GridFlowContainer<'T>, child: 'T) =
        parent.Add child
        parent

    static member (|+)(parent: #GridFlowContainer<'T>, children: 'T seq) =
        Seq.iter parent.Add children
        parent

    static member (|*)(parent: #GridFlowContainer<'T>, child: 'T) = parent.Add child
    static member (|*)(parent: #GridFlowContainer<'T>, children: 'T seq) = Seq.iter parent.Add children

    interface IHeight with
        member this.Height = content_height

    interface IResize with
        member this.OnSizeChanged
            with set v = size_change <- v

[<Extension>]
type GridFlowContainerExtensions =

    [<Extension>]
    static member Spacing(container: #GridFlowContainer<'T>, spacing: float32 * float32) : #GridFlowContainer<'T> =
        container.Spacing <- spacing
        container

    [<Extension>]
    static member Spacing(container: #GridFlowContainer<'T>, spacing: float32) : #GridFlowContainer<'T> =
        container.Spacing <- spacing, spacing
        container

    [<Extension>]
    static member Filter(container: #GridFlowContainer<'T>, filter: 'T -> bool) : unit =
        container.Filter <- filter

    [<Extension>]
    static member WithFilter(container: #GridFlowContainer<'T>, filter: 'T -> bool) : #GridFlowContainer<'T> =
        container.Filter <- filter
        container

    [<Extension>]
    static member Floating(container: #GridFlowContainer<'T>) : #GridFlowContainer<'T> =
        container.Floating <- true
        container

    [<Extension>]
    static member WrapNavigation(container: #GridFlowContainer<'T>, wrap: bool) : #GridFlowContainer<'T> =
        container.WrapNavigation <- wrap
        container