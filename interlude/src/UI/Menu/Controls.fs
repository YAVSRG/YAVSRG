namespace Interlude.UI

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude

type PageTextEntry(name: string, setting: Setting<string>) =
    inherit
        PageSetting(
            name,
            let entry =
                { new TextEntry(setting, "none", false) with
                    override this.OnFocus by_mouse =
                        base.OnFocus by_mouse
                        Style.hover.Play()
                }

            entry
            |+ Frame(
                Position = Position.DEFAULT.Shrink(-15.0f, 0.0f),
                Fill = K Color.Transparent,
                Border =
                    fun () ->
                        if entry.Selected then Colors.pink_accent
                        elif entry.Focused then Colors.yellow_accent
                        else Colors.grey_2
            )
        )

type OptionsMenuButton(label: string, on_click: unit -> unit) =
    inherit
        Container(
            NodeType.Button(fun () ->
                Style.click.Play()
                on_click ()
            )
        )

    member val IsHighlighted = K false with get, set
    member val Keybind = Bind.Dummy with get, set

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Init(parent) =
        this |* MouseListener().Button(this)
        base.Init(parent)

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        if this.Keybind.Pressed() then on_click()

    override this.Draw() =
        let is_highlighted = this.IsHighlighted()
        let trim_color =
            if is_highlighted then Colors.pink_accent
            elif this.Focused then Colors.black
            else Colors.shadow_1

        let color =
            if is_highlighted then Colors.pink_accent.O2
            elif this.Focused then Colors.shadow_2.O3
            else Colors.shadow_2.O2

        let text_color =
            if this.Focused then Colors.text_yellow_2
            elif is_highlighted then Colors.text
            else Colors.text_subheading

        Render.rect this.Bounds color
        Render.rect (this.Bounds.BorderB Style.PADDING) trim_color

        Text.fill_b (Style.font, label, this.Bounds.Shrink(Style.PADDING * 2.0f), text_color, Alignment.CENTER)