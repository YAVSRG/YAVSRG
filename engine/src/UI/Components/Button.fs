namespace Percyqaz.Flux.UI

open System.Drawing
open Percyqaz.Common
open Percyqaz.Flux.Input
open System.Runtime.CompilerServices

type Button(text: unit -> string, on_click: unit -> unit) as this =
    inherit
        Container(
            NodeType.Button(fun () ->
                if not (this.Disabled()) then
                    Style.click.Play()
                    on_click ()
            )
        )

    member val Hotkey: Hotkey = "none" with get, set
    member val Disabled: unit -> bool = K false with get, set
    member val Floating = false with get, set
    member val TextColor = K Colors.text with get, set
    member val Align = Alignment.CENTER with get, set

    new(text: string, on_click: unit -> unit) = Button(K text, on_click)

    override this.OnFocus(by_mouse: bool) : unit =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Init(parent: Widget) : unit =
        this
            .Add(
                Text(text)
                    .Align(this.Align)
                    .Color(
                        fun () ->
                            if this.Disabled() then Colors.text_greyout
                            elif this.Focused then Colors.text_yellow_2
                            else this.TextColor()
                    ),
                MouseListener().Button(this).Floating(),
                HotkeyListener(
                    this.Hotkey,
                    fun () ->
                        if not (this.Disabled()) then
                            Style.click.Play()
                            on_click ()
                )
            )

        base.Init parent

    override this.Focusable : bool = not (this.Disabled()) && base.Focusable

[<Extension>]
type ButtonExtensions =

    [<Extension>]
    static member Hotkey (button: Button, hotkey: Hotkey) : Button =
        button.Hotkey <- hotkey
        button

    [<Extension>]
    static member Disabled (button: Button) : Button =
        button.Disabled <- K true
        button

    [<Extension>]
    static member Disabled (button: Button, d: bool) : Button =
        button.Disabled <- K d
        button

    [<Extension>]
    static member Disabled (button: Button, d: unit -> bool) : Button =
        button.Disabled <- d
        button

    [<Extension>]
    static member Floating (button: Button) : Button =
        button.Floating <- true
        button

    [<Extension>]
    static member Align (button: Button, alignment: float32) : Button =
        button.Align <- alignment
        button

    [<Extension>]
    static member TextColor (button: Button, color: Color * Color) : Button =
        button.TextColor <- K color
        button

    [<Extension>]
    static member TextColor (button: Button, color: Color) : Button =
        button.TextColor <- K (color, Colors.shadow_2)
        button

    [<Extension>]
    static member TextColor (button: Button, color: unit -> Color * Color) : Button =
        button.TextColor <- color
        button

    [<Extension>]
    static member TextColor (button: Button, color: unit -> Color) : Button =
        button.TextColor <- fun () -> (color(), Colors.shadow_2)
        button