namespace Percyqaz.Flux.UI

open Percyqaz.Common
open Percyqaz.Flux.Input

/// Container that automatically positions its contents packed in a grid arrangement
type GridContainer<'T when 'T :> Widget>(row_height, columns: int) as this =
    inherit StaticWidget(NodeType.Switch (fun _ -> this.WhoShouldFocus))
    
    let children = ResizeArray<'T>()
    let mutable last_selected = 0
    let mutable refresh = false

    let i_to_xy i = (i % columns, i / columns)
    let xy_to_i (x, y) = x + y * columns

    let mutable content_height = 0.0f
    let contentChangeEvent = Event<float32>()
    member this.ContentHeightChanged = contentChangeEvent.Publish
            
    override this.Focus() = if children.Count > 0 then base.Focus()
    override this.Select() = if children.Count > 0 then base.Select()

    member val Spacing = (0.0f, 0.0f) with get, set
    
    member private this.WhoIsFocused : int option = Seq.tryFindIndex (fun (c: 'T) -> c.Focused) children
    member private this.WhoShouldFocus =
        if children.Count = 0 then failwithf "Tried to focus this %O with no children" this
        if last_selected >= children.Count then last_selected <- 0
        children.[last_selected]

    member this.PackContent() =
        let spacing_x, spacing_y = this.Spacing
        let width = (this.Bounds.Width - (float32 columns - 1.0f) * spacing_x) / float32 columns
        let mutable x = 0.0f
        let mutable y = 0.0f
        let mutable height = 0.0f
        for w in children do
            w.Position <- Position.Box(0.0f, 0.0f, x * (width + spacing_x), y * (row_height + spacing_y), width, row_height)
            height <- y * (row_height + spacing_y) + row_height
            x <- x + 1.0f
            if int x = columns then x <- 0.0f; y <- y + 1.0f
        if height <> content_height then
            content_height <- height
            contentChangeEvent.Trigger content_height

    member this.Add(child: 'T) =
        children.Add child
        if this.Initialised then 
            child.Init this
            refresh <- true

    member private this.Up() =
        match this.WhoIsFocused with
        | Some i ->
            let x, y = i_to_xy i
            let rows = children.Count / columns
            let mutable index = (y - 1) %% rows
            let mutable i = xy_to_i (x, index) 
            while index <> y && (children.Count <= i || not children.[i].Focusable) do
                index <- (index - 1) %% rows
                i <- xy_to_i (x, index) 
            last_selected <- i
            children.[i].Focus()
        | None -> ()
        
    member private this.Down() =
        match this.WhoIsFocused with
        | Some i ->
            let x, y = i_to_xy i
            let rows = children.Count / columns
            let mutable index = (y + 1) %% rows
            let mutable i = xy_to_i (x, index) 
            while index <> y && (children.Count <= i || not children.[i].Focusable) do
                index <- (index + 1) %% rows
                i <- xy_to_i (x, index) 
            last_selected <- i
            children.[i].Focus()
        | None -> ()

    member private this.Left() =
        match this.WhoIsFocused with
        | Some i ->
            let x, y = i_to_xy i
            let mutable index = (x - 1) %% columns
            let mutable i = xy_to_i (index, y) 
            while index <> x && (children.Count <= i || not children.[i].Focusable) do
                index <- (index - 1) %% columns
                i <- xy_to_i (index, y) 
            last_selected <- i
            children.[i].Focus()
        | None -> ()
            
    member private this.Right() =
        match this.WhoIsFocused with
        | Some i ->
            let x, y = i_to_xy i
            let mutable index = (x + 1) %% columns
            let mutable i = xy_to_i (index, y) 
            while index <> x && (children.Count <= i || not children.[i].Focusable) do
                index <- (index + 1) %% columns
                i <- xy_to_i (index, y) 
            last_selected <- i
            children.[i].Focus()
        | None -> ()
    
    override this.Init(parent: Widget) =
        base.Init parent
        this.PackContent()
        for c in children do
            c.Init this

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved || refresh)

        let moved = 
            if refresh then
                refresh <- false
                this.PackContent()
                true
            else moved

        for c in children do
            c.Update(elapsedTime, moved)

        if this.Focused then

            if (!|"up").Tapped() then this.Up()
            elif (!|"down").Tapped() then this.Down()
            elif (!|"left").Tapped() then this.Left()
            elif (!|"right").Tapped() then this.Right()
            elif (!|"select").Tapped() then
                match this.WhoIsFocused with
                | Some i -> last_selected <- i; children.[i].Select()
                | None -> ()

    override this.Draw() =
        for c in children do
            c.Draw()

    static member (|+) (parent: #GridContainer<'T>, child: 'T) = parent.Add child; parent
    static member (|*) (parent: #GridContainer<'T>, child: 'T) = parent.Add child
