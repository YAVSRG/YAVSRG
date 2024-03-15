namespace Percyqaz.Flux.UI

/// Container whose content can be swapped for other widgets
type SwapContainer(current: Widget) as this =
    inherit StaticWidget(NodeType.Container(fun () -> Some this.Current))

    let mutable current = current
    let mutable swapped_last_frame = false

    new() = SwapContainer(Dummy())

    member this.Current
        with get () = current
        and set (child) =
            let old_child = current
            current <- child

            if this.Initialised then
                if not child.Initialised then
                    child.Init this
                else
                    assert (child.Parent = this)

            if old_child.Focused then
                child.Focus false

            swapped_last_frame <- true

    override this.Init(parent) =
        base.Init parent

        current.Init this

    override this.Draw() = current.Draw()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        let moved =
            if swapped_last_frame then
                swapped_last_frame <- false
                true
            else
                moved

        current.Update(elapsed_ms, moved)
