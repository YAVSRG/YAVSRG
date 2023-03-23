namespace Interlude.UI.Menu

open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
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

    /// N for Name -- Shorthand for getting localised name of a setting `s`
    let N (s: string) = L ("options." + s + ".name")
    /// T for Tooltip -- Shorthand for getting localised tooltip of a setting `s`
    let T (s: string) = L ("options." + s + ".tooltip")
    /// E for Editing -- Shorthand for getting localised title of page when editing object named `name`
    let E (name: string) = Localisation.localiseWith [name] "misc.edit"

    let PRETTYTEXTWIDTH = 500.0f
    let PRETTYHEIGHT = 80.0f
    let PRETTYWIDTH = 1200.0f

type TooltipRegion(localisedText) =
    inherit StaticWidget(NodeType.None)

    member val Hotkey = None with get, set

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        if Mouse.hover this.Bounds && (!|"tooltip").Tapped() then
            let c = 
                (if this.Hotkey.IsSome then Callout.Normal.Hotkey(this.Hotkey.Value) else Callout.Normal)
                    .Body(localisedText)
                    .Icon(Icons.info)
            Notifications.tooltip ((!|"tooltip"), this, c)

    override this.Draw() = ()

type TooltipContainer(localisedText, child: Widget) =
    inherit StaticContainer(NodeType.Switch(fun _ -> child))

    member val Hotkey = None with get, set

    override this.Init(parent: Widget) =
        this
        |+ TooltipRegion(localisedText, Hotkey = this.Hotkey)
        |* child
        base.Init parent

    member this.WithPosition(pos) = this.Position <- pos; this

[<AutoOpen>]
module Tooltip =
    type Widget with
        //member this.Tooltip(localisedText) = TooltipContainer(localisedText, this)
        member this.Tooltip(localisedText, hotkey) = TooltipContainer(localisedText, this, Hotkey = Some hotkey)