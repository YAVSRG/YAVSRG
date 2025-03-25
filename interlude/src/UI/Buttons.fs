namespace Interlude.UI

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI

type InlaidButton(label_func: unit -> string, on_click: unit -> unit, icon: string) =
    inherit
        Container(
            NodeType.Button(fun () ->
                Style.click.Play()
                on_click ()
            )
        )

    static member HEIGHT = 55.0f

    new (label: string, on_click: unit -> unit, icon: string) = InlaidButton(K label, on_click, icon)

    member val Hotkey : Hotkey = "none" with get, set
    member val HoverText : string = label_func() with get, set
    member val HoverIcon : string = icon with get, set
    member val UnfocusedColor = Colors.text_greyout with get, set

    override this.Init(parent) =
        this
        |+ MouseListener().Button(this)
        |* HotkeyListener(
            this.Hotkey,
            fun () ->
                Style.click.Play()
                on_click ()
        )

        base.Init parent

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Draw() =

        let text =
            if this.Focused then
                if this.HoverIcon = "" then this.HoverText
                else sprintf "%s %s" this.HoverIcon this.HoverText
            elif icon = "" then label_func()
            else sprintf "%s %s" icon (label_func())

        Render.rect this.Bounds (Colors.shadow_1.O2)

        Text.fill_b (
            Style.font,
            text,
            this.Bounds.Shrink(10.0f, 5.0f),
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

    type TabButton(label: string, on_click: unit -> unit, is_disabled: unit -> bool, is_chosen: unit -> bool) =
        inherit Button(label, on_click, Disabled = is_disabled)

        override this.Draw() =
            if is_chosen() then
                Render.rect (this.Bounds.BorderT(Style.PADDING).ShrinkR(Style.PADDING)) Colors.grey_2.O2
            else
                Render.rect (this.Bounds.SliceB(this.Bounds.Height + Style.PADDING)) (if this.Focused then Colors.yellow_accent.O1 else Colors.shadow_1.O1)
                Render.rect (this.Bounds.BorderB(Style.PADDING).ShrinkR(Style.PADDING)) Colors.grey_2.O2
            Render.rect (this.Bounds.SliceR(Style.PADDING).Expand(0.0f, Style.PADDING)) Colors.grey_2.O2
            if this.Focused then
                Render.rect (this.Bounds.SliceB(Style.PADDING).Shrink(20.0f, 0.0f)) Colors.yellow_accent.O3
            base.Draw()

    // alternative designed to represent tabs on a tabbed container or view
    let create_tabs (options: RadioButtonOptions<'T>) : GridFlowContainer<TabButton> =
        GridFlowContainer(options.Height, options.Options.Length, Spacing = (0.0f, 0.0f), WrapNavigation = false)
        |+ seq {
            for value, label, disabled in options.Options do
                yield TabButton(
                    label,
                    (fun () -> options.Setting.Set value),
                    disabled,
                    (fun () -> options.Setting.Value = value)
                )
        }

    // todo: alternative designed to look like actual radio buttons