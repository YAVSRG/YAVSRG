namespace Interlude.UI

open System.Drawing
open System.Runtime.CompilerServices
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI

type InlaidButton(label_func: unit -> string, on_click: unit -> unit) =
    inherit
        Container(
            NodeType.Button(fun () ->
                Style.click.Play()
                on_click ()
            )
        )

    static member HEIGHT = 55.0f

    new (label: string, on_click: unit -> unit) = InlaidButton(K label, on_click)

    member val Hotkey : Hotkey = "none" with get, set
    member val Icon : string = "" with get, set
    member val HoverText : string = label_func() with get, set
    member val HoverIcon : string = "" with get, set
    member val TextColor = Colors.text_greyout with get, set

    override this.Init(parent) =
        this
            .Add(
                MouseListener().Button(this),
                HotkeyListener(this.Hotkey, fun () ->
                    Style.click.Play()
                    on_click ()
                )
            )

        base.Init parent

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Draw() =

        Render.rect this.Bounds (if this.Focused then Colors.shadow_1.O2 else Colors.shadow_2.O2)

        let text =
            if this.Focused then
                if this.HoverIcon = "" then this.HoverText
                else sprintf "%s %s" this.HoverIcon this.HoverText
            elif this.Icon = "" then label_func()
            else sprintf "%s %s" this.Icon (label_func())

        Text.fill_b (
            Style.font,
            text,
            this.Bounds.Shrink(10.0f, 5.0f),
            (if this.Focused then
                 Colors.text_yellow_2
             else
                 this.TextColor),
            Alignment.CENTER
        )

        base.Draw()

[<Extension>]
type InlaidButtonExtensions =

    [<Extension>]
    static member Hotkey(button: InlaidButton, hotkey: Hotkey) : InlaidButton =
        button.Hotkey <- hotkey
        button

    [<Extension>]
    static member Icon(button: InlaidButton, icon: string) : InlaidButton =
        button.Icon <- icon
        button.HoverIcon <- icon
        button

    [<Extension>]
    static member HoverIcon(button: InlaidButton, icon: string) : InlaidButton =
        button.HoverIcon <- icon
        button

    [<Extension>]
    static member HoverText(button: InlaidButton, text: string) : InlaidButton =
        button.HoverText <- text
        button

    [<Extension>]
    static member TextColor(button: InlaidButton, color: Color) : InlaidButton =
        button.TextColor <- (color, Colors.shadow_2)
        button

    [<Extension>]
    static member TextColor(button: InlaidButton, color: Color * Color) : InlaidButton =
        button.TextColor <- color
        button