namespace Interlude.UI.Components.Selection

open Interlude.UI

(*
    Fancy selection framework
*)

type Selectable() =
    inherit Widget1()

    (*
        - There is only one item hovered per widget
        - This one item can be marked as selected
        Invariants:
            - (1) Only at most 1 leaf can be hovered
            - (2) Selected leaves are a subset of hovered leaves, so at most 1 leaf can be selected
            - (3) Existence of a hovered leaf implies all its ancestors are selected
            - (4) Existence of a hovered leaf implies all its non-ancestors are not hovered
    *)

    let mutable hoverChild: Selectable option = None
    let mutable hoverSelected: bool = false

    abstract member SParent: Selectable option
    default this.SParent =
        match this.Parent with
        | Some p ->
            match p with
            | :? Selectable as p -> Some p
            | _ -> None
        | None -> None

    member this.SelectedChild
        with get() = if hoverSelected then hoverChild else None
        and set(value) =
            match value with
            | Some newChild ->
                match this.SelectedChild with
                | Some currentChild ->
                    if newChild <> currentChild then
                        currentChild.OnDeselect()
                        currentChild.OnDehover()
                        hoverChild <- value
                        newChild.OnSelect()
                | None ->
                    hoverChild <- value
                    hoverSelected <- true
                    match this.SParent with
                    | Some p -> p.SelectedChild <- Some this
                    | None -> ()
                    newChild.OnSelect()
            | None -> this.HoverChild <- None

    member this.HoverChild
        with get() = hoverChild
        and set(value) =
            match this.HoverChild with
            | Some c ->
                if hoverSelected then c.OnDeselect()
                if Some c <> value then c.OnDehover()
            | None -> ()
            hoverChild <- value
            hoverSelected <- false
            if value.IsSome && not this.Selected then this.Selected <- true

    member this.Selected
        with get() =
            match this.SParent with
            | Some p -> p.SelectedChild = Some this
            | None -> true
        and set(selected) =
            match this.SParent with
            | Some p -> if selected then p.SelectedChild <- Some this elif this.Hover then p.HoverChild <- Some this
            | None -> ()

    member this.Hover
        with get() =
            match this.SParent with
            | Some p -> p.HoverChild = Some this
            | None -> true
        and set(value) =
            match this.SParent with
            | Some p -> if value then p.HoverChild <- Some this elif this.Hover then p.HoverChild <- None
            | None -> ()

    abstract member OnSelect: unit -> unit
    default this.OnSelect() = ()

    abstract member OnDeselect: unit -> unit
    default this.OnDeselect() = ()

    abstract member OnDehover: unit -> unit
    default this.OnDehover() = this.HoverChild <- None