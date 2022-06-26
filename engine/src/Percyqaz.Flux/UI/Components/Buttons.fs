namespace Percyqaz.Flux.UI

open System
open System.Drawing
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics

type Button(text: string, onClick: unit -> unit, hotkey: Hotkey) as this =
    inherit StaticContainer(NodeType.Leaf)
    
    // todo: icons, hover colors

    do
        this
        |+ Text(
            text,
            Align = Alignment.CENTER,
            Color = fun () -> (Style.highlight(255, 0.8f), Color.Black))
        |+ Clickable(this.Select, OnHover = fun b -> if b && not this.Focused then this.Focus())
        |* HotkeyAction(hotkey, onClick)
    
    override this.OnSelected() =
        base.OnSelected()
        onClick()

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        if this.Selected then this.Focus()