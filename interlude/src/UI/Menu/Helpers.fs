namespace Interlude.UI.Menu

open Percyqaz.Flux.UI
open Prelude.Common
open Interlude.Utils

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