namespace Interlude.UI

open System.Drawing
open System.Runtime.CompilerServices
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI

type AngledButton(label_func: unit -> string, on_click: unit -> unit, background_color: unit -> Color) as this =
    inherit
        Container(
            NodeType.Button(fun () ->
                if not (this.Disabled()) then
                    Style.click.Play()
                    on_click ()
            )
        )

    static member HEIGHT : float32 = 50.0f
    static member LEAN_RATIO : float32 = 0.5f
    static member LEAN_AMOUNT : float32 = AngledButton.HEIGHT * AngledButton.LEAN_RATIO

    new (label: string, on_click: unit -> unit, background_color: unit -> Color) = AngledButton(K label, on_click, background_color)

    new (label_func: unit -> string, on_click: unit -> unit, background_color: PaletteColor) = AngledButton(label_func, on_click, !%background_color)
    new (label: string, on_click: unit -> unit, background_color: PaletteColor) = AngledButton(K label, on_click, !%background_color)

    new (label_func: unit -> string, on_click: unit -> unit, background_color: Color) = AngledButton(label_func, on_click, K background_color)
    new (label: string, on_click: unit -> unit, background_color: Color) = AngledButton(K label, on_click, K background_color)

    member val Hotkey : Hotkey = "none" with get, set
    member val LeanLeft : bool = true with get, set
    member val LeanRight : bool = true with get, set
    member val Disabled : unit -> bool = K false with get, set
    member val Floating : bool = false with get, set
    member val TextColor : unit -> Color * Color = K Colors.text_subheading with get, set

    override this.Draw() : unit =
        Render.quad_points
            (this.Bounds.Left, this.Bounds.Top)
            (this.Bounds.Right + (if this.LeanRight then AngledButton.LEAN_AMOUNT else 0.0f), this.Bounds.Top)
            (this.Bounds.Right, this.Bounds.Bottom)
            (this.Bounds.Left - (if this.LeanLeft then AngledButton.LEAN_AMOUNT else 0.0f), this.Bounds.Bottom)
            (background_color())

        Text.fill_b (
            Style.font,
            label_func (),
            this.Bounds.ShrinkX(Style.PADDING),
            (
                if this.Disabled() then Colors.text_greyout
                elif this.Focused then Colors.text_yellow_2
                else this.TextColor()
            ),
            0.5f
        )
        base.Draw()

    override this.Init(parent: Widget) =
        this
            .Add(
                MouseListener()
                    .Button(this)
                    .Floating(this.Floating),

                HotkeyListener(this.Hotkey, fun () ->
                    if not (this.Disabled()) then
                        Style.click.Play()
                        on_click ()
                )
            )

        base.Init parent

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Focusable : bool = not (this.Disabled()) && base.Focusable

[<Extension>]
type LeaningButtonExtensions =

    [<Extension>]
    static member Hotkey (button: AngledButton, hotkey: Hotkey) : AngledButton =
        button.Hotkey <- hotkey
        button

    [<Extension>]
    static member LeanLeft (button: AngledButton, l: bool) : AngledButton =
        button.LeanLeft <- l
        button

    [<Extension>]
    static member LeanRight (button: AngledButton, l: bool) : AngledButton =
        button.LeanRight <- l
        button

    [<Extension>]
    static member Disabled (button: AngledButton) : AngledButton =
        button.Disabled <- K true
        button

    [<Extension>]
    static member Disabled (button: AngledButton, d: bool) : AngledButton =
        button.Disabled <- K d
        button

    [<Extension>]
    static member Disabled (button: AngledButton, d: unit -> bool) : AngledButton =
        button.Disabled <- d
        button

    [<Extension>]
    static member Floating (button: AngledButton) : AngledButton =
        button.Floating <- true
        button

    [<Extension>]
    static member TextColor (button: AngledButton, color: Color * Color) : AngledButton =
        button.TextColor <- K color
        button

    [<Extension>]
    static member TextColor (button: AngledButton, color: Color) : AngledButton =
        button.TextColor <- K (color, Colors.shadow_2)
        button

    [<Extension>]
    static member TextColor (button: AngledButton, color: unit -> Color * Color) : AngledButton =
        button.TextColor <- color
        button

    [<Extension>]
    static member TextColor (button: AngledButton, color: unit -> Color) : AngledButton =
        button.TextColor <- fun () -> (color(), Colors.shadow_2)
        button