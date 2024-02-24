namespace Percyqaz.Flux.UI

open Percyqaz.Flux.Input

[<AbstractClass>]
type ISelection(node_type: NodeType) =

    member this.NodeType = node_type
    abstract member FocusTree: ISelection list

    abstract member OnFocus: unit -> unit
    abstract member OnUnfocus: unit -> unit
    abstract member OnSelected: unit -> unit
    abstract member OnDeselected: unit -> unit

    abstract member Focusable: bool

    default this.Focusable =
        match node_type with
        | NodeType.None -> false
        | NodeType.Container f ->
            match f() with
            | Some child -> child.Focusable
            | None -> false
        | _ -> true

and [<RequireQualifiedAccess>] NodeType =
    | None
    | Leaf
    | FocusTrap
    | Container of (unit -> ISelection option)
    | Button of (unit -> unit)
    member this._IsNone =
        match this with
        | None -> true
        | _ -> false

module Selection =

    let mutable private current_tree: ISelection list = []
    let mutable private leaf_is_selected: bool = false
    let mutable private current_clamp_tree: ISelection list = []

    let get_focused_element () = List.tryHead current_tree

    // Test if a proposed tree is rooted under the required parent
    let private check_clamp (tree: ISelection list) =
        if current_clamp_tree.IsEmpty then
            true
        else
            let l = current_clamp_tree.Length
            tree.Length >= l && List.skip (tree.Length - l) tree = current_clamp_tree

    let private diff (xs: ISelection list) (ys: ISelection list) =
        let mutable xs = xs
        let mutable ys = ys
        let mutable difference_found = false

        while not (difference_found || xs.IsEmpty || ys.IsEmpty) do
            if xs.Head = ys.Head then
                xs <- List.tail xs
                ys <- List.tail ys
            else
                difference_found <- true

        for x in List.rev xs do
            x.OnUnfocus()

        for y in ys do
            y.OnFocus()

    let rec private focus_tree (tree: ISelection list) =

        if not (check_clamp tree) then
            ()
        else

        if leaf_is_selected then
            match List.tryHead current_tree with
            | Some w -> w.OnDeselected()
            | None -> ()

            leaf_is_selected <- false

        diff (List.rev current_tree) (List.rev tree)
        current_tree <- tree

    let rec private select_tree (tree: ISelection list) =
        if not (check_clamp tree) then
            ()
        else

        if tree <> current_tree && leaf_is_selected then
            match List.tryHead current_tree with
            | Some w -> w.OnDeselected()
            | None -> ()

        diff (List.rev current_tree) (List.rev tree)

        if tree <> current_tree || not leaf_is_selected then
            match List.tryHead tree with
            | Some w -> w.OnSelected()
            | None -> ()

        leaf_is_selected <- true
        current_tree <- tree


    let rec focus (item: ISelection) =

        match get_focused_element () with
        | Some x when leaf_is_selected && item <> x && (match x.NodeType with NodeType.FocusTrap -> true | _ -> false) -> ()
        | _ ->

        match item.NodeType with
        | NodeType.None -> focus_tree []
        | NodeType.Leaf -> focus_tree item.FocusTree
        | NodeType.FocusTrap -> focus_tree item.FocusTree
        | NodeType.Container f ->
            match f () with
            | Some child -> focus child
            | None -> focus_tree item.FocusTree
        | NodeType.Button a -> focus_tree item.FocusTree

    let rec select (item: ISelection) =
        match item.NodeType with
        | NodeType.None -> select_tree []
        | NodeType.Leaf -> select_tree item.FocusTree
        | NodeType.FocusTrap -> select_tree item.FocusTree
        | NodeType.Container f ->
            match f () with
            | Some child -> select child
            | None -> select_tree item.FocusTree
        | NodeType.Button a ->
            focus_tree item.FocusTree
            a ()

    let rec clamp_to (item: ISelection) = current_clamp_tree <- item.FocusTree

    let unclamp () = current_clamp_tree <- []

    let rec up () =
        if leaf_is_selected then
            focus_tree current_tree
        else
            match current_tree with
            | [] -> ()
            | head :: rest ->
                head.OnUnfocus()
                current_tree <- rest

                match List.tryHead rest with
                | Some h ->
                    match h.NodeType with
                    | NodeType.None -> up ()
                    | NodeType.Leaf -> ()
                    | NodeType.FocusTrap -> ()
                    | NodeType.Container _ -> up ()
                    | NodeType.Button _ -> ()
                | None -> ()

    let clear () = focus_tree []

// drag and drop system is unfinished
// todo: finish it

type DragAndDropEvent<'T> =
    {
        Origin: float32 * float32
        Target: float32 * float32
        Payload: 'T
    }

type IDropzone<'T> =
    abstract member Drop: DragAndDropEvent<'T> -> unit

type DragAndDropAction<'T>(payload: 'T) =
    let origin = Mouse.pos ()

    let mutable _acceptor: IDropzone<'T> option = None

    member this.Accept(acceptor: IDropzone<'T>) = _acceptor <- Some acceptor

    member this.Unaccept(acceptor: IDropzone<'T>) =
        if _acceptor.IsSome && _acceptor.Value = acceptor then
            _acceptor <- None

    member this.Drop() =
        match _acceptor with
        | Some a ->
            let target = Mouse.pos ()

            a.Drop
                {
                    Origin = origin
                    Target = target
                    Payload = payload
                }
        | None -> ()

module DragAndDrop =

    let mutable private ref: DragAndDropAction<obj> option = None
    let mutable private ty: System.Type = null

    let drag (payload: 'T) =
        match ref with
        | Some r -> ()
        | None ->
            let action = DragAndDropAction payload
            ref <- Some(unbox action: DragAndDropAction<obj>)
            ty <- typeof<'T>

    let drop () =
        match ref with
        | Some r -> r.Drop()
        | None -> ()

    let accept<'T> (zone: IDropzone<'T>) =
        match ref with
        | Some r when typeof<'T> = ty -> (unbox r: DragAndDropAction<'T>).Accept(zone)
        | _ -> ()

    let unaccept<'T> (zone: IDropzone<'T>) =
        match ref with
        | Some r when typeof<'T> = ty -> (unbox r: DragAndDropAction<'T>).Unaccept(zone)
        | _ -> ()

    let current<'T> () =
        match ref with
        | Some r when typeof<'T> = ty -> Some(unbox r: DragAndDropAction<'T>)
        | _ -> None
