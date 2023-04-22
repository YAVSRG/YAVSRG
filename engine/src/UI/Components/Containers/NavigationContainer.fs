namespace Percyqaz.Flux.UI

open Percyqaz.Flux.Input

/// Container that provides navigation and selection using arrow keys + enter
/// Content is assumed to be positioned in a layout that fits the navigation
module SwitchContainer =

    [<AbstractClass>]
    type Base<'T when 'T :> Widget>() as this = 
        inherit StaticWidget(NodeType.Switch (fun _ -> this.WhoShouldFocus))

        let children = ResizeArray<'T>()
        let mutable last_selected = 0
        
        override this.Focus() = if children.Count > 0 then base.Focus()
        override this.Select() = if children.Count > 0 then base.Select()

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
            for i = children.Count - 1 downto 0 do
                children.[i].Update(elapsedTime, moved)
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