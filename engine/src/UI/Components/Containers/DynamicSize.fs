namespace Percyqaz.Flux.UI

// todo: maybe split into 3 interfaces: IWidth, IHeight, IResize
type DynamicSize =

    abstract member Size: float32
    abstract OnSizeChanged: (unit -> unit) with set
