namespace Percyqaz.Flux.UI

open System.Runtime.CompilerServices

/// Each frame, evaluate `condition` - Only show contained component if true
[<Sealed>]
type Conditional<'T when 'T :> Widget>(condition: unit -> bool, child: 'T) =
    inherit StaticWidget(NodeType.Container(fun () -> Some child))

    let mutable moved_store = false

    override this.Init(parent: Widget) =
        base.Init parent
        child.Init this

    override this.Draw() =
        if condition () then
            child.Draw()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if condition () then
            child.Update(elapsed_ms, moved || moved_store)
            moved_store <- false
        else
            moved_store <- moved_store || moved

    override this.Focusable = condition () && base.Focusable

    member this.Child : 'T = child

[<Extension>]
type ConditionalExtensions =

    [<Extension>]
    static member Conditional (widget: #Widget, condition: unit -> bool) : Conditional<#Widget> =
        Conditional(condition, widget)