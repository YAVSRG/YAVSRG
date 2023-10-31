namespace Percyqaz.Flux.UI

open System.Linq
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.Utils

type private GridFlowItem<'T when 'T :> Widget> =
    {
        Widget: 'T
        mutable Visible: bool
        mutable X: int
        mutable Y: int
    }

/// Container that automatically positions its contents packed in a grid arrangement
type GridFlowContainer<'T when 'T :> Widget>(row_height, columns: int) as this =
    inherit StaticWidget(NodeType.Switch(fun _ -> this.WhoShouldFocus))

    let mutable spacing = 0.0f, 0.0f
    let mutable filter: 'T -> bool = K true
    let mutable refresh = false
    let mutable last_selected = 0
    let children = ResizeArray<GridFlowItem<'T>>()

    let mutable content_height = 0.0f
    let content_change_ev = Event<float32>()
    member this.ContentHeightChanged = content_change_ev.Publish

    override this.Focus() =
        if children.Count > 0 then
            base.Focus()

    override this.Select() =
        if children.Count > 0 then
            base.Select()

    member this.Clear() =
        require_ui_thread ()
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

    member private this.WhoShouldFocus =
        if children.Count = 0 then
            failwithf "Tried to focus this %O with no children" this

        if last_selected >= children.Count then
            last_selected <- 0

        children.[last_selected].Widget

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
            content_change_ev.Trigger content_height

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
                children.[i].Widget.Focus()
            | None -> ()
        | None -> ()

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
                children.[i].Widget.Focus()
            | None -> ()
        | None -> ()

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
            | None -> false

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
            | None -> false

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
            | None -> false

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
            | None -> false

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved || refresh)

        let moved =
            if moved || refresh then
                refresh <- false
                this.PackContent()
                true
            else
                false

        for { Widget = c; Visible = visible } in children do
            if visible && (moved || this.Floating || c.VisibleBounds.Visible) then
                c.Update(elapsedTime, moved)

        if this.Focused then

            if this.CanUp() && (!| "up").Tapped() then
                this.Up()

            if this.CanDown() && (!| "down").Tapped() then
                this.Down()

            if this.CanLeft() && (!| "left").Tapped() then
                this.Left()

            if this.CanRight() && (!| "right").Tapped() then
                this.Right()

            if (!| "select").Tapped() then
                match this.WhoIsFocused with
                | Some i ->
                    last_selected <- i
                    children.[i].Widget.Select()
                | None -> ()

    override this.Draw() =
        for { Widget = c; Visible = visible } in children do
            if visible && (this.Floating || c.VisibleBounds.Visible) then
                c.Draw()

    member this.Add(child: 'T) : unit =
        require_ui_thread ()

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

    static member (|+)(parent: #GridFlowContainer<_>, child: #Widget) =
        parent.Add child
        parent

    static member (|*)(parent: #GridFlowContainer<_>, child: #Widget) = parent.Add child
