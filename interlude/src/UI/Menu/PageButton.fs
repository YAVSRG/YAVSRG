namespace Interlude.UI

open System.Runtime.CompilerServices
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude

type PageButton(localised_text: string, on_click: unit -> unit) as this =
    inherit
        Container(
            NodeType.Button(fun _ ->
                if not (this.Disabled()) then
                    Style.click.Play()
                    on_click ()
            )
        )

    member val Icon : string = "" with get, set
    member val Hotkey : Bind = Bind.Dummy with get, set
    member val TextColor : unit -> Color * Color = K Colors.text with get, set
    member val Disabled = K false with get, set

    override this.Init(parent: Widget) =
        this
            .With(
                Text(
                    if this.Icon <> "" then
                        sprintf "%s %s  >" this.Icon localised_text
                    else
                        sprintf "%s  >" localised_text
                )
                    .Color(fun () ->
                        if this.Disabled() then Colors.text_greyout
                        elif this.Focused then Colors.text_yellow_2
                        else this.TextColor()
                    )
                    .Align(Alignment.LEFT)
                    .Position(Position.Shrink(Style.PADDING)),

                MouseListener().Button(this)
            )
            .AddConditional(
                this.Hotkey <> Bind.Dummy,
                Text(sprintf "%s: %O" (%"misc.hotkeyhint") this.Hotkey)
                    .Color(Colors.text_cyan)
                    .Align(Alignment.RIGHT)
                    .Position(Position.Shrink(10.0f, 5.0f))
            )

        base.Init parent

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        if this.Hotkey.Pressed() && not (this.Disabled()) then
            Style.click.Play()
            on_click ()

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Draw() =
        if this.Focused then
            Render.rect this.Bounds Colors.yellow_accent.O1

        base.Draw()

    static member Once(localised_text: string, action: unit -> unit) =
        let mutable clicked = false
        PageButton(
            localised_text,
            (fun () ->
                clicked <- true
                action()
            ),
            Disabled = fun () -> clicked
        )

    static member Once(localised_text: string, action: unit -> unit, disabled: unit -> bool) =
        let mutable clicked = false
        PageButton(
            localised_text,
            (fun () ->
                clicked <- true
                action()
            ),
            Disabled = fun () -> clicked || disabled()
        )

[<Extension>]
type PageButtonExtensions =

    [<Extension>]
    static member Hotkey (button: PageButton, hotkey: Hotkey) : PageButton =
        button.Hotkey <- %%hotkey
        button

    [<Extension>]
    static member Hotkey (button: PageButton, hotkey: Bind) : PageButton =
        button.Hotkey <- hotkey
        button

    [<Extension>]
    static member Disabled (button: PageButton) : PageButton =
        button.Disabled <- K true
        button

    [<Extension>]
    static member Disabled (button: PageButton, d: bool) : PageButton =
        button.Disabled <- K d
        button

    [<Extension>]
    static member Disabled (button: PageButton, d: unit -> bool) : PageButton =
        button.Disabled <- d
        button

    [<Extension>]
    static member TextColor (button: PageButton, color: Color * Color) : PageButton =
        button.TextColor <- K color
        button

    [<Extension>]
    static member TextColor (button: PageButton, color: Color) : PageButton =
        button.TextColor <- K (color, Colors.shadow_2)
        button

    [<Extension>]
    static member TextColor (button: PageButton, color: unit -> Color * Color) : PageButton =
        button.TextColor <- color
        button

    [<Extension>]
    static member TextColor (button: PageButton, color: unit -> Color) : PageButton =
        button.TextColor <- fun () -> (color(), Colors.shadow_2)
        button