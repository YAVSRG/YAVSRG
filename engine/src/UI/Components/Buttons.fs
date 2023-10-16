namespace Percyqaz.Flux.UI

open Percyqaz.Common
open Percyqaz.Flux.Input

type Button(text: unit -> string, onClick: unit -> unit) as this =
    inherit StaticContainer(NodeType.Button (fun () -> if not (this.Disabled()) then Style.click.Play(); onClick()))

    member val Hotkey : Hotkey = "none" with get, set
    member val Disabled : unit -> bool = K false with get, set
    member val Floating = false with get, set

    new(text: string, onClick: unit -> unit) = Button(K text, onClick)

    override this.OnFocus() = Style.hover.Play(); base.OnFocus()

    override this.Init(parent: Widget) =
        this
        |+ Text(
            text,
            Align = Alignment.CENTER,
            Color = fun () -> 
                if this.Disabled() then Colors.text_greyout
                elif this.Focused then Colors.text_yellow_2
                else Colors.text)
        |+ Clickable((fun () -> if not (this.Disabled()) then this.Select()), 
            Floating = this.Floating,
            OnHover = fun b -> if b && not (this.Disabled()) then this.Focus())
        |* HotkeyAction(this.Hotkey, fun () -> Style.click.Play(); onClick())
        base.Init parent

type IconButton(text: unit -> string, icon: string, iconSize: float32, onClick: unit -> unit) as this =
    inherit StaticContainer(NodeType.Button (fun () -> if not (this.Disabled()) then Style.click.Play(); onClick()))

    member val Hotkey : Hotkey = "none" with get, set
    member val Disabled : unit -> bool = K false with get, set
    member val HoverIcon = icon with get, set

    new(text: string, icon, iconSize, onClick: unit -> unit) = IconButton(K text, icon, iconSize, onClick)
    
    override this.OnFocus() = Style.hover.Play(); base.OnFocus()

    override this.Init(parent: Widget) =
        this
        |+ Text(
            (fun () -> if this.Focused then this.HoverIcon else icon),
            Align = Alignment.CENTER,
            Color = fun () -> 
                if this.Disabled() then Colors.text_greyout
                elif this.Focused then Colors.text_yellow_2
                else Colors.text
            ,
            Position = Position.SliceLeft iconSize)
        |+ Text(
            text,
            Align = Alignment.CENTER,
            Color = fun () -> 
                if this.Disabled() then Colors.text_greyout
                elif this.Focused then Colors.text_yellow_2
                else Colors.text
            ,
            Position = Position.TrimLeft iconSize)
        |+ Clickable((fun () -> if not (this.Disabled()) then this.Select()), OnHover = fun b -> if b && not (this.Disabled()) then this.Focus())
        |* HotkeyAction(this.Hotkey, fun () -> Style.click.Play(); onClick())
        base.Init parent