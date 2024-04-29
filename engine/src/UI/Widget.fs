namespace Percyqaz.Flux.UI

open Percyqaz.Flux.Utils
open Percyqaz.Flux.Graphics

[<AbstractClass>]
type Widget(node_type) =
    inherit ISelection(node_type)

    let mutable _parent = None
    let mutable focused = false
    let mutable focused_by_mouse = false
    let mutable selected = false

    member this.Selected = selected
    member this.Focused = focused
    member this.FocusedByMouse = focused_by_mouse

    member this.Parent =
        match _parent with
        | Some p -> p
        | None -> failwithf "%O has no parent (probably due to not calling init)" this

    member val Initialised = false with get, set
    member val Bounds = Rect.ZERO with get, set
    member val VisibleBounds = Rect.ZERO with get, set

    abstract member Position: Position with set
    abstract member Update: float * bool -> unit

    abstract member Draw: unit -> unit

    // The container must call this before calling Draw or Update
    abstract member Init: Widget -> unit

    default this.Init(parent: Widget) =
        if this.Initialised then
            failwithf "This widget %O has already been initialised" this

        this.Initialised <- true
        _parent <- Some parent

    override this.FocusTree: ISelection list =
        if not this.NodeType._IsNone then
            this :: this.Parent.FocusTree
        else
            this.Parent.FocusTree

    member this.Focus(by_mouse: bool) =
        if this.Focusable then
            Selection.focus by_mouse this

    member this.Select(by_mouse: bool) =
        if this.Focusable then
            Selection.select by_mouse this

    override this.OnFocus by_mouse =
        if not focused then
            focused <- true
            focused_by_mouse <- by_mouse

    override this.OnUnfocus _ =
        focused <- false
        focused_by_mouse <- false

    override this.OnSelected _ = selected <- true
    override this.OnDeselected _ = selected <- false

    override this.ToString() =
        if _parent.IsNone then "*" else _parent.Value.ToString()
        + " > "
        + this.GetType().Name
    
    static member inline (|>>)(child: Widget, constructor: NodeType -> 'T) : 'T =
        constructor (NodeType.Container (fun () -> Some child)) |+ child

[<AbstractClass>]
type StaticWidget(node_type) =
    inherit Widget(node_type)

    let mutable pos = Position.Default

    override this.Position
        with set (value) =
            pos <- value

            if this.Initialised then
                this.UpdateBounds()

    member private this.UpdateBounds() =
        this.Bounds <- Position.calculate pos this.Parent.Bounds
        this.VisibleBounds <- this.Bounds.Intersect this.Parent.VisibleBounds

    override this.Update(elapsed_ms, moved) =
        if moved then
            this.UpdateBounds()

    override this.Init(parent: Widget) =
        base.Init parent
        this.UpdateBounds()

type Container(node_type) =
    inherit StaticWidget(node_type)

    let children = ResizeArray<Widget>()

    let mutable position_changed = false

    override this.Position
        with set (value) =
            base.set_Position value
            position_changed <- true

    override this.Draw() =
        for c in children do
            c.Draw()

    override this.Update(elapsed_ms, moved) =
        let moved = if position_changed then position_changed <- false; true else moved
        base.Update(elapsed_ms, moved)

        // children are updated in reverse order
        // ensures the visually topmost children have priority for events like being clicked on
        for i = children.Count - 1 downto 0 do
            children.[i].Update(elapsed_ms, moved)

    member this.Add(child: #Widget) =
        require_ui_thread ()
        children.Add child

        if this.Initialised then
            child.Init this

    member this.Remove(child: Widget) : bool =
        require_ui_thread ()
        children.Remove child

    override this.Init(parent: Widget) =
        base.Init parent

        for c in children do
            c.Init this

    static member (|+)(parent: #Container, child: #Widget) =
        parent.Add child
        parent

    static member (|+)(parent: #Container, children: #Widget seq) =
        Seq.iter parent.Add children
        parent

    static member (|*)(parent: #Container, child: #Widget) = parent.Add child
    static member (|*)(parent: #Container, children: #Widget seq) = Seq.iter parent.Add children

type private SlideablePosition(pos: Position) =
    let mutable pos = pos

    let mutable snapped_last_frame = false

    let left_offset = Animation.Fade(fst pos.Left)
    let left_anchor = Animation.Fade(snd pos.Left)
    let top_offset = Animation.Fade(fst pos.Top)
    let top_anchor = Animation.Fade(snd pos.Top)
    let right_offset = Animation.Fade(fst pos.Right)
    let right_anchor = Animation.Fade(snd pos.Right)
    let bottom_offset = Animation.Fade(fst pos.Bottom)
    let bottom_anchor = Animation.Fade(snd pos.Bottom)

    let anim =
        Animation.fork
            [
                left_offset
                left_anchor
                top_offset
                top_anchor
                right_offset
                right_anchor
                bottom_offset
                bottom_anchor
            ]

    member this.Moving = left_offset.Moving || (if snapped_last_frame then snapped_last_frame <- false; true else false)

    member this.Position
        with get () =
            if this.Moving then
                {
                    Left = (left_offset.Value, left_anchor.Value)
                    Top = (top_offset.Value, top_anchor.Value)
                    Right = (right_offset.Value, right_anchor.Value)
                    Bottom = (bottom_offset.Value, bottom_anchor.Value)
                }
                : Position
            else
                pos
        and set (value) =
            pos <- value
            left_offset.Target <- fst pos.Left
            left_anchor.Target <- snd pos.Left
            top_offset.Target <- fst pos.Top
            top_anchor.Target <- snd pos.Top
            right_offset.Target <- fst pos.Right
            right_anchor.Target <- snd pos.Right
            bottom_offset.Target <- fst pos.Bottom
            bottom_anchor.Target <- snd pos.Bottom

    member this.Snap() =
        left_offset.Snap()
        left_anchor.Snap()
        top_offset.Snap()
        top_anchor.Snap()
        right_offset.Snap()
        right_anchor.Snap()
        bottom_offset.Snap()
        bottom_anchor.Snap()
        snapped_last_frame <- true

    member this.Update(elapsed_ms) =
        if this.Moving then
            anim.Update elapsed_ms

type SlideContainer(node_type) =
    inherit Widget(node_type)

    let pos = SlideablePosition(Position.Default)
    let children = ResizeArray<Widget>()

    override this.Position
        with set (value) =
            pos.Position <- value

            if not this.Initialised then
                pos.Snap()

    member this.Moving = pos.Moving

    member this.SnapPosition() =
        pos.Snap()

        if this.Initialised then
            this.UpdateBounds()

    member private this.UpdateBounds() =
        this.Bounds <- Position.calculate pos.Position this.Parent.Bounds
        this.VisibleBounds <- this.Bounds.Intersect this.Parent.VisibleBounds

    override this.Draw() =
        for c in children do
            c.Draw()

    override this.Update(elapsed_ms, moved) =
        pos.Update elapsed_ms
        let moved = moved || pos.Moving

        if moved then
            this.UpdateBounds()

        for i = children.Count - 1 downto 0 do
            children.[i].Update(elapsed_ms, moved)

    member this.Add(child: #Widget) =
        require_ui_thread ()
        children.Add child

        if this.Initialised then
            child.Init this

    override this.Init(parent: Widget) =
        base.Init parent
        this.UpdateBounds()

        for c in children do
            c.Init this

    static member (|+)(parent: #SlideContainer, child: #Widget) =
        parent.Add child
        parent

    static member (|+)(parent: #SlideContainer, children: #Widget seq) =
        Seq.iter parent.Add children
        parent

    static member (|*)(parent: #SlideContainer, child: #Widget) = parent.Add child
    static member (|*)(parent: #SlideContainer, children: #Widget seq) = Seq.iter parent.Add children

[<AbstractClass>]
type Overlay(node_type: NodeType) =
    inherit Widget(node_type)

    override this.Position
        with set _ = failwith "Position can not be set for overlay components"

    override this.Init(parent: Widget) =
        base.Init parent
        this.Bounds <- Viewport.bounds
        this.VisibleBounds <- Viewport.bounds

    override this.Update(elapsed_ms, moved) =
        if moved then
            this.Bounds <- Viewport.bounds
            this.VisibleBounds <- Viewport.bounds
