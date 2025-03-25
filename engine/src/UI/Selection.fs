namespace Percyqaz.Flux.UI

open Percyqaz.Flux.Input

[<AbstractClass>]
type ISelection(node_type: NodeType) =

    member this.NodeType = node_type
    abstract member FocusTree: ISelection list

    abstract member OnFocus: bool -> unit
    abstract member OnUnfocus: bool -> unit
    abstract member OnSelected: bool -> unit
    abstract member OnDeselected: bool -> unit

    abstract member Focusable: bool

    default this.Focusable =
        match node_type with
        | NodeType.None -> false
        | NodeType.Container f ->
            match f () with
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
    let mutable private focused_by_mouse: bool = false
    let mutable private leaf_is_selected: bool = false
    let mutable private current_clamp_tree: ISelection list = []

    let get_focused_element () = List.tryHead current_tree

    let get_selected_element () =
        if leaf_is_selected then List.tryHead current_tree else None

    // Test if a proposed tree is rooted under the required parent
    let private check_clamp (by_mouse: bool) (tree: ISelection list) : bool =

        if
            by_mouse
            && (
                match get_selected_element () with
                | Some x when
                    List.tryHead tree <> Some x
                    && (
                        match x.NodeType with
                        | NodeType.FocusTrap -> true
                        | _ -> false
                    )
                    ->
                    true
                | _ -> false
            )
        then
            false
        elif current_clamp_tree.IsEmpty then
            true
        else
            let l = current_clamp_tree.Length
            tree.Length >= l && List.skip (tree.Length - l) tree = current_clamp_tree

    let private diff (by_mouse: bool) (xs: ISelection list) (ys: ISelection list) =
        let mutable xs = xs
        let mutable ys = ys
        let mutable difference_found = false

        while not (difference_found || xs.IsEmpty || ys.IsEmpty) do
            if xs.Head = ys.Head && by_mouse = focused_by_mouse then
                xs <- List.tail xs
                ys <- List.tail ys
            else
                difference_found <- true

        for x in List.rev xs do
            x.OnUnfocus by_mouse

        for y in ys do
            y.OnFocus by_mouse
        focused_by_mouse <- by_mouse

    let rec private focus_tree (by_mouse: bool) (tree: ISelection list) =

        if not (check_clamp by_mouse tree) then
            ()
        else

        if leaf_is_selected then
            match List.tryHead current_tree with
            | Some w -> w.OnDeselected by_mouse
            | None -> ()

            leaf_is_selected <- false

        diff by_mouse (List.rev current_tree) (List.rev tree)
        current_tree <- tree

    let rec private select_tree (by_mouse: bool) (tree: ISelection list) =
        if not (check_clamp by_mouse tree) then
            ()
        else

        if tree <> current_tree && leaf_is_selected then
            match List.tryHead current_tree with
            | Some w -> w.OnDeselected by_mouse
            | None -> ()

        diff by_mouse (List.rev current_tree) (List.rev tree)

        if tree <> current_tree || not leaf_is_selected then
            match List.tryHead tree with
            | Some w -> w.OnSelected by_mouse
            | None -> ()

        leaf_is_selected <- true
        current_tree <- tree

    let rec focus (by_mouse: bool) (item: ISelection) =
        match item.NodeType with
        | NodeType.None -> focus_tree by_mouse []
        | NodeType.Leaf -> focus_tree by_mouse item.FocusTree
        | NodeType.FocusTrap -> focus_tree by_mouse item.FocusTree
        | NodeType.Container f ->
            match f () with
            | Some child -> focus by_mouse child
            | None -> focus_tree by_mouse item.FocusTree
        | NodeType.Button _ -> focus_tree by_mouse item.FocusTree

    let rec select (by_mouse: bool) (item: ISelection) =
        match item.NodeType with
        | NodeType.None -> select_tree by_mouse []
        | NodeType.Leaf -> select_tree by_mouse item.FocusTree
        | NodeType.FocusTrap -> select_tree by_mouse item.FocusTree
        | NodeType.Container f ->
            match f () with
            | Some child -> select by_mouse child
            | None -> select_tree by_mouse item.FocusTree
        | NodeType.Button a ->
            focus_tree by_mouse item.FocusTree
            a ()

    let rec clamp_to (item: ISelection) = current_clamp_tree <- item.FocusTree

    let unclamp () = current_clamp_tree <- []

    let rec up (by_mouse: bool) =
        if leaf_is_selected then
            focus_tree by_mouse current_tree
        else
            match current_tree with
            | [] -> ()
            | head :: rest ->
                head.OnUnfocus by_mouse
                current_tree <- rest
                focused_by_mouse <- by_mouse

                match List.tryHead rest with
                | Some h ->
                    match h.NodeType with
                    | NodeType.None -> up by_mouse
                    | NodeType.Container _ ->
                        if not by_mouse then
                            up false
                    | _ -> ()
                | None -> ()

    let clear () = focus_tree false []

// drag and drop system is unfinished
// todo: finish it
// todo: or delete it

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