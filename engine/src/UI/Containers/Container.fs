namespace Percyqaz.Flux.UI

open Percyqaz.Common
open Percyqaz.Flux.Windowing

type Container(node_type: NodeType) =
    inherit StaticWidget(node_type)

    let children = ResizeArray<Widget>()

    let mutable position_changed = false

    override this.Position
        with set (value: Position) =
            base.set_Position value
            position_changed <- true

    override this.Draw() : unit =
        for c in children do
            c.Draw()

    override this.Update(elapsed_ms: float, moved: bool) : unit =
        let moved =
            if position_changed then
                position_changed <- false
                true
            else
                moved

        base.Update(elapsed_ms, moved)

        // children are updated in reverse order
        // ensures the visually topmost children have priority for events like being clicked on
        for i = children.Count - 1 downto 0 do
            children.[i].Update(elapsed_ms, moved)

    member this.Add(child: #Widget) : unit = (this :> IContainer<Widget>).Add(child)

    member this.Remove(child: Widget) : bool =
        (this :> IContainer<Widget>).Remove(child)

    override this.Init(parent: Widget) : unit =
        base.Init(parent)

        for c in children do
            c.Init(this)

    static member Create(child: Widget) : Container =
        Container(NodeType.Container(K(Some child))).With(child)

    interface IContainer<Widget> with

        member this.Add(child: Widget) : unit =
            assert GameThread.is_game_thread()
            children.Add(child)

            if this.Initialised then
                child.Init(this)

        member this.Remove(child: Widget) : bool =
            assert GameThread.is_game_thread()
            children.Remove(child)
