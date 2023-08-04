namespace Interlude.UI

open System.Drawing

module Style =

    open Percyqaz.Flux.UI

    type private ColorFunc = unit -> Color

    let highlight a : ColorFunc = fun () -> (!*Palette.HIGHLIGHT).O4a a
    let highlightL a : ColorFunc = fun () -> (!*Palette.LIGHT).O4a a
    let main a : ColorFunc = fun () -> (!*Palette.MAIN).O4a a
    let dark a : ColorFunc = fun () -> (!*Palette.DARK).O4a a
    let darkD a : ColorFunc = fun () -> (!*Palette.DARKER).O4a a