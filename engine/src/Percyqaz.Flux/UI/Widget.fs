namespace Percyqaz.Flux.UI

open Percyqaz.Flux.Graphics

[<AbstractClass>]
type Widget(nodeType) =
    inherit ISelection(nodeType)

    let mutable parent = None
    let mutable focused = false
    let mutable selected = false
    member this.Parent = parent.Value

    member this.Selected = selected
    member this.Focused = focused

    member val Initialised = false with get, set
    member val Bounds = Rect.ZERO with get, set
    member val VisibleBounds = Rect.ZERO with get, set

    abstract member Position : Position with set

    abstract member Update : float * bool -> unit

    abstract member Draw : unit -> unit

    // The container must call this before calling Draw or Update
    abstract member Init : Widget -> unit
    default this.Init(p: Widget) =
        if this.Initialised then failwithf "This widget %O has already been initialised" this
        this.Initialised <- true
        parent <- Some p

    override this.FocusTree : ISelection list = 
        if not this.NodeType._IsNone then this :: this.Parent.FocusTree
        else this.Parent.FocusTree

    override this.Focus() = if not this.NodeType._IsNone then Selection.select this
    override this.OnFocus() = focused <- true
    override this.OnUnfocus() = focused <- false

    override this.Select() = if this.NodeType._IsLeaf then Selection.select this
    override this.OnSelected() = assert focused; selected <- true
    override this.OnDeselected() = assert focused; selected <- false

    override this.ToString() =
        if parent.IsNone then "*" else parent.Value.ToString()
        + " > "
        + this.GetType().Name

[<AbstractClass>]
type StaticWidget(nodeType) =
    inherit Widget(nodeType)

    let mutable pos = Position.Default

    override this.Position with set(value) = pos <- value; if this.Initialised then this.UpdateBounds()

    member private this.UpdateBounds() =
        this.Bounds <- Position.calculate pos this.Parent.Bounds
        this.VisibleBounds <- this.Bounds.Intersect this.Parent.VisibleBounds

    override this.Update(elapsedTime, moved) =
        if moved then this.UpdateBounds()

    override this.Init(parent: Widget) =
        base.Init parent
        this.UpdateBounds()

type StaticContainer(nodeType) =
    inherit StaticWidget(nodeType)

    let children = ResizeArray<Widget>()

    override this.Draw() =
        for c in children do
            c.Draw()

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)

        for i = children.Count - 1 downto 0 do
            children.[i].Update(elapsedTime, moved)

    member this.Add(child: #Widget) =
        children.Add child
        if this.Initialised then child.Init this

    override this.Init(parent: Widget) =
        base.Init parent
        for c in children do
            c.Init this

    static member (|+) (parent: #StaticContainer, child: #Widget) = parent.Add child; parent
    static member (|*) (parent: #StaticContainer, child: #Widget) = parent.Add child

type private DynamicPosition(pos: Position) =
    let mutable pos = pos
    let mutable frames = 0

    let left_offset = Animation.Fade(fst pos.Left)
    let left_anchor = Animation.Fade(snd pos.Left)
    let top_offset = Animation.Fade(fst pos.Top)
    let top_anchor = Animation.Fade(snd pos.Top)
    let right_offset = Animation.Fade(fst pos.Right)
    let right_anchor = Animation.Fade(snd pos.Right)
    let bottom_offset = Animation.Fade(fst pos.Bottom)
    let bottom_anchor = Animation.Fade(snd pos.Bottom)

    let anim =
        Animation.Fork(
            left_offset, left_anchor,
            top_offset, top_anchor,
            right_offset, right_anchor,
            bottom_offset, bottom_anchor )

    member this.Moving = frames > 0

    member this.Position
        with get() = 
            if frames > 1 then
                { 
                    Left = (left_offset.Value, left_anchor.Value)
                    Top = (top_offset.Value, top_anchor.Value)
                    Right = (right_offset.Value, right_anchor.Value)
                    Bottom = (bottom_offset.Value, bottom_anchor.Value)
                } : Position
            else pos
        and set(value) = 
            pos <- value
            frames <- 180

    member this.Snap() =
        left_offset.Snap(); left_anchor.Snap()
        top_offset.Snap(); top_anchor.Snap()
        right_offset.Snap(); right_anchor.Snap()
        bottom_offset.Snap(); bottom_anchor.Snap()

    member this.Update(elapsedTime) =
        if frames > 0 then 
            frames <- frames - 1
            anim.Update(elapsedTime) |> ignore

[<AbstractClass>]
type DynamicContainer(nodeType) =
    inherit Widget(nodeType)

    let pos = DynamicPosition(Position.Default)
    let children = ResizeArray<Widget>()

    override this.Position with set(value) = pos.Position <- value; if not this.Initialised then pos.Snap()

    member private this.UpdateBounds() =
        this.Bounds <- Position.calculate pos.Position this.Parent.Bounds
        this.VisibleBounds <- this.Bounds.Intersect this.Parent.VisibleBounds

    override this.Draw() =
        for c in children do
            c.Draw()

    override this.Update(elapsedTime, moved) =
        pos.Update elapsedTime
        let moved = moved || pos.Moving
        if moved then this.UpdateBounds()

        for i = children.Count - 1 downto 0 do
            children.[i].Update(elapsedTime, moved)

    member this.Add(child: #Widget) =
        children.Add child
        if this.Initialised then child.Init this

    override this.Init(parent: Widget) =
        base.Init parent
        this.UpdateBounds()
        for c in children do
            c.Init this

    static member (|+) (parent: #DynamicContainer, child: #Widget) = parent.Add child; parent
    static member (|*) (parent: #DynamicContainer, child: #Widget) = parent.Add child