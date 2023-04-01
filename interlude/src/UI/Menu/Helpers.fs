namespace Interlude.UI.Menu

open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Prelude.Common
open Interlude.Utils
open Interlude.UI

[<AutoOpen>]
module Helpers =

    let column() = SwitchContainer.Column<Widget>()
    let row() = SwitchContainer.Row<Widget>()

    let refreshRow number cons =
        let r = SwitchContainer.Row()
        let refresh() =
            r.Clear()
            let n = number()
            for i in 0 .. (n - 1) do
                r.Add(cons i n)
        refresh()
        r, refresh

    /// E for Editing -- Shorthand for getting localised title of page when editing object named `name`
    let E (name: string) = Localisation.localiseWith [name] "misc.edit"

    let PRETTYTEXTWIDTH = 425.0f
    let PRETTYHEIGHT = 70.0f
    let PRETTYWIDTH = 1200.0f

type Tooltip(content: Callout) =
    inherit StaticWidget(NodeType.None)

    let content = content.Icon(Icons.info)
    
    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        if Mouse.hover this.Bounds then
            Notifications.tooltip_available <- true
            if (!|"tooltip").Tapped() then Notifications.tooltip ((!|"tooltip"), this, content)

    override this.Draw() = ()

    static member Info(feature: string) =
        Callout.Normal
            .Title(L (sprintf "%s.name" feature))
            .Body(L (sprintf "%s.tooltip" feature))

    static member Info(feature: string, hotkey: Hotkey) =
        Callout.Normal
            .Title(L (sprintf "%s.name" feature))
            .Body(L (sprintf "%s.tooltip" feature))
            .Hotkey(hotkey)

[<AutoOpen>]
module Tooltip =
    type StaticContainer with
        member this.Tooltip(content: Callout) = this |+ Tooltip(content)