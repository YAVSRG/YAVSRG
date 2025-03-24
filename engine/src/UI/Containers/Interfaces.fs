namespace Percyqaz.Flux.UI

open System
open System.Runtime.CompilerServices

type IWidth =
    abstract member Width: float32

type IHeight =
    abstract member Height: float32

type IResize =
    abstract member OnSizeChanged: (unit -> unit) with set

type IContainer<'T when 'T :> Widget> =
    abstract member Add: 'T -> unit
    abstract member Remove: 'T -> bool

[<Extension>]
type ContainerExtensions =

    [<Extension>]
    static member Add (container: #IContainer<'T>, one: 'T, two: 'T, [<ParamArray>] many: 'T array) : unit =
        container.Add one
        container.Add two
        Array.iter container.Add many

    [<Extension>]
    static member Add (container: #IContainer<'T>, children: 'T seq) : unit =
        Seq.iter container.Add children

    [<Extension>]
    static member With (container: #IContainer<'T>, [<ParamArray>] children: 'T array) : #IContainer<'T> =
        Array.iter container.Add children
        container

    [<Extension>]
    static member With (container: #IContainer<'T>, children: 'T seq) : #IContainer<'T> =
        Seq.iter container.Add children
        container