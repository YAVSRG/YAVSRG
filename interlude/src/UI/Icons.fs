namespace Interlude.UI

type Icons = Percyqaz.Flux.Resources.FeatherIcons

[<AutoOpen>]
module IconOperators =

    let inline (.+) (icon: string) (fmt: PrintfFormat<_, _, _, _, _>) : 'a -> string =
        (sprintf fmt) >> (fun s -> icon + " " + s)
