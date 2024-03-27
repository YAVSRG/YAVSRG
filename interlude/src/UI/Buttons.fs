namespace Interlude.UI

open OpenTK.Mathematics
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI

type StylishButton(on_click, label_func: unit -> string, color_func) as this =
    inherit
        Container(
            NodeType.Button(fun () ->
                if not (this.Disabled()) then
                    Style.click.Play()
                    on_click ()
            )
        )

    member val Hotkey: Hotkey = "none" with get, set
    member val TiltLeft = true with get, set
    member val TiltRight = true with get, set
    member val Disabled: unit -> bool = K false with get, set

    member val TextColor =
        fun () -> (if this.Focused then Colors.yellow_accent else Colors.grey_1), Colors.shadow_2 with get, set

    override this.Draw() =
        let h = this.Bounds.Height

        Draw.untextured_quad
            (Quad.create
             <| Vector2(this.Bounds.Left, this.Bounds.Top)
             <| Vector2(this.Bounds.Right + (if this.TiltRight then h * 0.5f else 0.0f), this.Bounds.Top)
             <| Vector2(this.Bounds.Right, this.Bounds.Bottom)
             <| Vector2(this.Bounds.Left - (if this.TiltLeft then h * 0.5f else 0.0f), this.Bounds.Bottom))
            (color_func () |> Quad.color)

        Text.fill_b (Style.font, label_func (), this.Bounds, (if this.Disabled() then Colors.text_greyout else this.TextColor()), 0.5f)
        base.Draw()

    override this.Init(parent: Widget) =
        this |+ Clickable.Focus this
        |* HotkeyAction(
            this.Hotkey,
            fun () ->
                if not (this.Disabled()) then
                    Style.click.Play()
                    on_click ()
        )

        base.Init parent

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Focusable = not (this.Disabled()) && base.Focusable

    static member Selector<'T>(label: string, values: ('T * string) array, setting: Setting<'T>, color_func) =
        let mutable current = array.IndexOf(values |> Array.map fst, setting.Value)
        current <- max 0 current

        StylishButton(
            (fun () ->
                current <- (current + 1) % values.Length
                setting.Value <- fst values.[current]
            ),
            (fun () -> sprintf "%s %s" label (snd values.[current])),
            color_func
        )

type InlaidButton(label, action, icon) =
    inherit
        Container(
            NodeType.Button(fun () ->
                Style.click.Play()
                action ()
            )
        )

    member val Hotkey = "none" with get, set
    member val HoverText = label with get, set
    member val HoverIcon = icon with get, set
    member val UnfocusedColor = Colors.text_greyout with get, set

    override this.Init(parent) =
        this |+ Clickable.Focus this
        |* HotkeyAction(
            this.Hotkey,
            fun () ->
                Style.click.Play()
                action ()
        )

        base.Init parent

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Draw() =
        let area = this.Bounds.TrimBottom(15.0f)

        let text =
            if this.Focused then
                sprintf "%s %s" this.HoverIcon this.HoverText
            else
                sprintf "%s %s" icon label

        Draw.rect area (Colors.shadow_1.O2)

        Text.fill_b (
            Style.font,
            text,
            area.Shrink(10.0f, 5.0f),
            (if this.Focused then
                 Colors.text_yellow_2
             else
                 this.UnfocusedColor),
            Alignment.CENTER
        )

        base.Draw()

module RadioButtons =

    type RadioButtonOptions<'T> =
        {
            Setting: Setting<'T>
            Options: ('T * string * (unit -> bool)) array
            Height: float32
        }

    let create (options: RadioButtonOptions<'T>) =
        GridFlowContainer(options.Height, options.Options.Length, Spacing = (options.Height * 0.5f, 0.0f), WrapNavigation = false)
        |+ seq {
            let mutable i = 0
            for value, label, disabled in options.Options do
                yield StylishButton(
                    (fun () -> options.Setting.Set value),
                    K label,
                    (let i = i in fun () ->
                        if options.Setting.Value = value then
                            Colors.cyan
                        else
                            if i % 2 = 0 then Colors.black.O3 else Colors.shadow_2.O3
                    ),
                    Disabled = disabled,
                    TiltRight = (i + 1 < options.Options.Length),
                    TiltLeft = (i > 0)
                )
                i <- i + 1
        }