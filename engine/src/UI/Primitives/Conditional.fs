namespace Percyqaz.Flux.UI

/// Each frame, evaluate `condition` - Only show contained component if true
[<Sealed>]
type Conditional<'T when 'T :> Widget>(condition: unit -> bool, child: 'T) =
    inherit StaticWidget(NodeType.Container(fun () -> Some child))

    override this.Init(parent: Widget) =
        base.Init parent
        child.Init this

    override this.Draw() =
        if condition () then
            child.Draw()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if moved || condition () then
            child.Update(elapsed_ms, moved)

    override this.Focusable = condition () && base.Focusable

    member this.Child = child

[<AutoOpen>]
module ConditionalExtensions =

    type Widget with
        member this.Conditional(condition: unit -> bool) = Conditional(condition, this)