namespace Interlude.UI

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude

type PageSetting(localised_text, widget: Widget) as this =
    inherit Container(NodeType.Container(fun _ -> Some this.Child))

    let mutable widget = widget

    member this.Child
        with get () = widget
        and set (w: Widget) =
            let old_widget = widget
            widget <- w
            w.Position <- Position.ShrinkL(PAGE_LABEL_WIDTH).Shrink(Style.PADDING)

            if this.Initialised then
                w.Init this

                if old_widget.Focused then
                    w.Focus false

    member this.Label = localised_text

    override this.Init(parent) =
        this
        |* Text(
            localised_text + ":",
            Color = (fun () -> (if widget.Focused then Colors.text_yellow_2 else Colors.text)),
            Align = Alignment.LEFT,
            Position =
                Position
                    .Box(0.0f, 0.0f, 0.0f, 0.0f, PAGE_LABEL_WIDTH - 10.0f, PAGE_ITEM_HEIGHT)
                    .Shrink(Style.PADDING)
        )

        base.Init parent
        widget.Position <- Position.ShrinkL(PAGE_LABEL_WIDTH).Shrink(Style.PADDING)
        widget.Init this

    override this.Draw() =
        if widget.Selected then
            Render.rect (widget.Bounds.Expand(15.0f, Style.PADDING)) Colors.pink_accent.O2
        elif widget.Focused then
            Render.rect (widget.Bounds.Expand(15.0f, Style.PADDING)) Colors.yellow_accent.O1

        base.Draw()
        widget.Draw()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        widget.Update(elapsed_ms, moved)

type PageButton(localised_text, on_click) as this =
    inherit
        Container(
            NodeType.Button(fun _ ->
                if not (this.Disabled()) then
                    Style.click.Play()
                    on_click ()
            )
        )

    member val Icon = "" with get, set
    member val Hotkey = Bind.Dummy with get, set

    override this.Init(parent: Widget) =
        this
        |+ Text(
            K(
                if this.Icon <> "" then
                    sprintf "%s %s  >" this.Icon localised_text
                else
                    sprintf "%s  >" localised_text
            ),
            Color =
                (fun () ->
                    if not (this.Disabled()) then
                        (if this.Focused then Colors.text_yellow_2 else Colors.text)
                    else
                        Colors.text_greyout
                ),
            Align = Alignment.LEFT,
            Position = Position.Shrink(Style.PADDING)
        )
        |+ Clickable.Focus this
        |* seq {
            if this.Hotkey <> Bind.Dummy then
                yield Text(sprintf "%s: %O" (%"misc.hotkeyhint") this.Hotkey,
                    Color = K Colors.text_cyan,
                    Align = Alignment.RIGHT,
                    Position = Position.Shrink(10.0f, 5.0f)
                )
        }

        base.Init parent

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        if this.Hotkey.Tapped() && not (this.Disabled()) then
            Style.click.Play()
            on_click ()

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Draw() =
        if this.Focused then
            Render.rect this.Bounds Colors.yellow_accent.O1

        base.Draw()

    member val Disabled = K false with get, set

    static member Once(localised_text, action) =
        let mutable clicked = false
        PageButton(
            localised_text,
            (fun () ->
                clicked <- true
                action()
            ),
            Disabled = fun () -> clicked
        )

    static member Once(localised_text, action, disabled: unit -> bool) =
        let mutable clicked = false
        PageButton(
            localised_text,
            (fun () ->
                clicked <- true
                action()
            ),
            Disabled = fun () -> clicked || disabled()
        )

type PageTextEntry(name, setting) =
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

type OptionsMenuButton(label: string, width: float32, on_click: unit -> unit) =
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
        this |* Clickable.Focus this
        base.Init(parent)

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        if this.Keybind.Tapped() then on_click()

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

    interface IWidth with
        member this.Width = width

[<AutoOpen>]
module Helpers =

    let page_container () : NavigationContainer.Column =
        NavigationContainer.Column(WrapNavigation = false, Position = Position.Shrink(PAGE_MARGIN_X, PAGE_MARGIN_Y))

    let refreshable_row (get_count: unit -> int) (constructor: int -> int -> Widget) : NavigationContainer.Row * (unit -> unit) =
        let r = NavigationContainer.Row()

        let refresh () =
            r.Clear()
            let n = get_count ()

            for i in 0 .. (n - 1) do
                r.Add(constructor i n)

        refresh ()
        r, refresh