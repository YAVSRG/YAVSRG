namespace Percyqaz.Flux.UI

open Percyqaz.Common
open Percyqaz.Flux.Input

/// Container whose content can be swapped for other widgets
type SwapContainer() as this =
    inherit StaticWidget(NodeType.Switch (fun () -> this.Current))

    let mutable current : Widget = Unchecked.defaultof<_>
    let mutable justSwapped = false

    member this.Current
        with get() = current
        and set(v) =
            current <- v
            if this.Initialised then
                if not current.Initialised then current.Init this
                else assert(current.Parent = this)
            justSwapped <- true

    override this.Init(parent) =
        base.Init parent
        if isNull (current :> obj) then 
            Logging.Error("SwapContainer was not given child element before init")
        else current.Init this

    override this.Draw() =
        current.Draw()

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        let moved = if justSwapped then justSwapped <- false; true else moved
        current.Update(elapsedTime, moved)