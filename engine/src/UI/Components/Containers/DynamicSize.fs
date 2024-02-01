namespace Percyqaz.Flux.UI

type DynamicSize =

    abstract member Size: float32
    abstract OnSizeChanged : (unit -> unit) with set