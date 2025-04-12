namespace Interlude.UI

type Icons = Percyqaz.Flux.Icons.Feather

[<AutoOpen>]
module IconOperators =

    let inline (.+) (icon: string) (fmt: PrintfFormat<_,_,_,_,_>) : 'a -> string =
        (sprintf fmt) >> (fun s -> icon + " " + s)