namespace Interlude.UI.Components.Selection.Buttons

open System
open Percyqaz.Common
open Prelude.Common
open Percyqaz.Flux.UI
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Components.Selection

type ButtonBase(onClick) as this =
    inherit Selectable()
    do this.Add(Clickable((fun () -> this.Selected <- true), fun b -> if b then this.Hover <- true))

    override this.OnSelect() =
        base.OnSelect()
        onClick()
        this.Selected <- false

type LittleButton(label, onClick) as this =
    inherit ButtonBase(onClick)
    do
        this.Add(Frame(Color.FromArgb(80, 255, 255, 255), ()))
        this.Add(TextBox(label, (fun () -> ((if this.Hover then Style.color(255, 1.0f, 0.7f) else Color.White), Color.Black)), 0.5f))

    static member FromEnum<'T when 'T: enum<int>>(label: string, setting: Setting<'T>, onClick) =
        let names = Enum.GetNames(typeof<'T>)
        let values = Enum.GetValues(typeof<'T>) :?> 'T array
        let mutable i = array.IndexOf(values, setting.Value)
        LittleButton((fun () -> sprintf "%s: %s" label names.[i]),
            (fun () -> i <- (i + 1) % values.Length; setting.Value <- values.[i]; onClick()))

type IconButton(icon, onClick) as this =
    inherit ButtonBase(onClick)
    do TextBox(K icon, (fun () -> (if this.Hover then Style.color(255, 1.0f, 0.4f) else Color.White), Color.Black), 0.5f) |> this.Add