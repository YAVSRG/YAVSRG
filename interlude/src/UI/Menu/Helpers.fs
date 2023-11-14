namespace Interlude.UI.Menu

open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude.Common
open Interlude.Utils
open Interlude.UI

[<AutoOpen>]
module Helpers =

    let column () = NavigationContainer.Column<Widget>()
    let row () = NavigationContainer.Row<Widget>()

    let refreshable_row number cons =
        let r = NavigationContainer.Row()

        let refresh () =
            r.Clear()
            let n = number ()

            for i in 0 .. (n - 1) do
                r.Add(cons i n)

        refresh ()
        r, refresh

    let PRETTYTEXTWIDTH = 425.0f
    let PRETTYHEIGHT = 70.0f
    let PRETTYWIDTH = 1200.0f

type Tooltip(content: Callout) =
    inherit StaticWidget(NodeType.None)

    let content = content.Icon(Icons.info)

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if Mouse.hover this.Bounds then
            Notifications.tooltip_available <- true

            if (%%"tooltip").Tapped() then
                Notifications.tooltip ((%%"tooltip"), this, content)

    override this.Draw() = ()

    static member Info(feature: string) =
        Callout.Normal
            .Title(%(sprintf "%s.name" feature))
            .Body(%(sprintf "%s.tooltip" feature))

    static member Info(feature: string, hotkey: Hotkey) =
        Callout.Normal
            .Title(%(sprintf "%s.name" feature))
            .Body(%(sprintf "%s.tooltip" feature))
            .Hotkey(hotkey)

[<AutoOpen>]
module Tooltip =
    type StaticContainer with
        member this.Tooltip(content: Callout) = this |+ Tooltip(content)
