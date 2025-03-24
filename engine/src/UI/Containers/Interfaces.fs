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
    static member AddConditional (container: #IContainer<'T>, condition: bool, [<ParamArray>] children: 'T array) : unit =
        if condition then Array.iter container.Add children

    [<Extension>]
    static member AddConditional (container: #IContainer<'T>, condition: bool, children: 'T seq) : unit =
        if condition then Seq.iter container.Add children

    [<Extension>]
    static member AddConditional (container: #IContainer<Widget>, condition: unit -> bool, [<ParamArray>] children: Widget array) : unit =
        for c in children do
            container.Add (c.Conditional(condition))

    [<Extension>]
    static member AddConditional (container: #IContainer<Widget>, condition: unit -> bool, children: Widget seq) : unit =
        for c in children do
            container.Add (c.Conditional(condition))

    [<Extension>]
    static member With (container: #IContainer<'T>, [<ParamArray>] children: 'T array) : #IContainer<'T> =
        Array.iter container.Add children
        container

    [<Extension>]
    static member With (container: #IContainer<'T>, children: 'T seq) : #IContainer<'T> =
        Seq.iter container.Add children
        container

    [<Extension>]
    static member WithConditional (container: #IContainer<'T>, condition: bool, [<ParamArray>] children: 'T array) : #IContainer<'T> =
        if condition then Array.iter container.Add children
        container

    [<Extension>]
    static member WithConditional (container: #IContainer<'T>, condition: bool, children: 'T seq) : #IContainer<'T> =
        if condition then Seq.iter container.Add children
        container

    [<Extension>]
    static member WithConditional (container: #IContainer<Widget>, condition: unit -> bool, [<ParamArray>] children: Widget array) : #IContainer<Widget> =
        for c in children do
            container.Add (c.Conditional(condition))
        container

    [<Extension>]
    static member WithConditional (container: #IContainer<Widget>, condition: unit -> bool, children: Widget seq) : #IContainer<Widget> =
        for c in children do
            container.Add (c.Conditional(condition))
        container