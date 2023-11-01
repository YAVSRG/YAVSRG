namespace Percyqaz.Flux.UI

open Percyqaz.Common

/// Container whose content can be swapped for other widgets
type SwapContainer() as this =
    inherit StaticWidget(NodeType.Switch(fun () -> this.Current))

    let mutable current: Widget = Unchecked.defaultof<_>
    let mutable swapped_last_frame = false

    override this.Focusable = current.Focusable

    member this.Current
        with get () = current
        and set (v) =
            current <- v

            if this.Initialised then
                if not current.Initialised then
                    current.Init this
                else
                    assert (current.Parent = this)

            swapped_last_frame <- true

    override this.Init(parent) =
        base.Init parent

        if isNull (current :> obj) then
            Logging.Error("SwapContainer was not given child element before init")
        else
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
