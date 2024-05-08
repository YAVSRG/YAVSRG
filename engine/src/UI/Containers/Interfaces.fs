namespace Percyqaz.Flux.UI

type IWidth =
    abstract member Width: float32

type IHeight =
    abstract member Height: float32

type IResize =
    abstract member OnSizeChanged: (unit -> unit) with set
