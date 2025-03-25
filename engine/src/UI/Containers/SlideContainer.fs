namespace Percyqaz.Flux.UI

open Percyqaz.Flux.Windowing

type private SlideablePosition(pos: Position) =
    let mutable pos = pos

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

    member this.Moving = left_offset.Moving

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

    member this.Update(elapsed_ms) =
        if this.Moving then
            anim.Update elapsed_ms

type SlideContainer(node_type) =
    inherit Widget(node_type)

    let pos = SlideablePosition(Position.DEFAULT)
    let children = ResizeArray<Widget>()
    let mutable snapped_last_frame = false

    override this.Position
        with set (value) =
            pos.Position <- value

            if not this.Initialised then
                pos.Snap()

    member this.Moving = pos.Moving

    member this.SnapPosition() =
        pos.Snap()
        if this.Initialised then
            snapped_last_frame <- true

    member private this.UpdateBounds() =
        this.Bounds <- Position.calculate pos.Position this.Parent.Bounds
        this.VisibleBounds <- this.Bounds.IntersectWith this.Parent.VisibleBounds

    override this.Draw() =
        for c in children do
            c.Draw()

    override this.Update(elapsed_ms, moved) =
        pos.Update elapsed_ms
        let moved = moved || pos.Moving || (if snapped_last_frame then snapped_last_frame <- false; true else false)

        if moved then
            this.UpdateBounds()

        for i = children.Count - 1 downto 0 do
            children.[i].Update(elapsed_ms, moved)

    member this.Add(child: #Widget) = (this :> IContainer<Widget>).Add child
    member this.Remove(child: Widget) : bool = (this :> IContainer<Widget>).Remove child

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

    interface IContainer<Widget> with

        member this.Add (child: Widget) : unit =
            assert(GameThread.is_game_thread())
            children.Add child

            if this.Initialised then
                child.Init this

        member this.Remove (child: Widget) : bool =
            assert(GameThread.is_game_thread())
            children.Remove child