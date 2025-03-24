namespace Percyqaz.Flux.UI

open System.Drawing
open System.Runtime.CompilerServices
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input

module Alignment =
    let LEFT = 0.0f
    let CENTER = 0.5f
    let RIGHT = 1.0f

[<Sealed>]
type Text(text_func: unit -> string) =
    inherit StaticWidget(NodeType.None)

    new(text: string) = Text(K text)

    member val Align = Alignment.CENTER with get, set
    member val Color = K Colors.text with get, set

    override this.Draw() =
        Text.fill_b (Style.font, text_func (), this.Bounds, this.Color(), this.Align)

    override this.Init(parent) = base.Init parent

[<Extension>]
type TextExtensions =
    [<Extension>]
    static member Align (text: Text, alignment: float32) : Text =
        text.Align <- alignment
        text
    [<Extension>]
    static member Color (text: Text, color: Color * Color) : Text =
        text.Color <- K color
        text
    [<Extension>]
    static member Color (text: Text, color: unit -> Color * Color) : Text =
        text.Color <- color
        text

[<Sealed>]
type Clickable(on_left_click: unit -> unit) =
    inherit StaticWidget(NodeType.None)

    let mutable hover = false

    member val OnLeftClick : unit -> unit = on_left_click with get, set
    member val OnRightClick : unit -> unit = ignore with get, set
    member val OnHover : bool -> unit = ignore with get, set
    member val Floating : bool = false with get, set

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        let was_hovering = hover
        hover <- Mouse.hover (if this.Floating then this.Bounds else this.VisibleBounds)

        if was_hovering && not hover then
            this.OnHover false
        elif not was_hovering && hover && Mouse.moved_recently () then
            this.OnHover true
        elif hover then
            if Mouse.left_clicked () then
                this.OnLeftClick()

            if Mouse.right_clicked () then
                this.OnRightClick()

    override this.Draw() = ()

    static member Focus(w: Widget) =
        Clickable(
            (fun () -> w.Select true),
            OnHover =
                fun b ->
                    if b && not w.Focused then
                        w.Focus true
                    elif not b && w.FocusedByMouse then
                        Selection.up true
        )

[<Sealed>]
type HotkeyAction(hotkey: Hotkey, action: unit -> unit) =
    inherit StaticWidget(NodeType.None)

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if (%%hotkey).Pressed() then
            action ()

    override this.Draw() = ()

[<Sealed>]
type HotkeyHoldAction(hotkey: Hotkey, on_tap: unit -> unit, on_hold: unit -> unit) =
    inherit StaticWidget(NodeType.None)

    let HOLD_TIME_MS = 200.0

    let mutable hold_time_remaining = 0.0

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if (%%hotkey).Pressed() then
            on_tap ()
            hold_time_remaining <- HOLD_TIME_MS

        if (%%hotkey).Held() then
            if hold_time_remaining > 0.0 then
                hold_time_remaining <- hold_time_remaining - elapsed_ms

                if hold_time_remaining < 0 then
                    on_hold ()

    override this.Draw() = ()

[<Sealed>]
type Image(sprite: Sprite) =
    inherit StaticWidget(NodeType.None)

    member val Sprite = sprite with get, set
    member val StretchToFill = true with get, set

    override this.Draw() =
        if this.StretchToFill then
            Render.sprite this.Bounds Color.White this.Sprite
        else
            Render.sprite (Sprite.fill this.Bounds this.Sprite) Color.White this.Sprite

[<Sealed>]
type Conditional<'T when 'T :> Widget>(condition: unit -> bool, child: 'T) =
    inherit StaticWidget(NodeType.Container(fun () -> Some child))

    override this.Init(parent: Widget) =
        base.Init parent
        child.Init this

    override this.Draw() =
        if condition () then
            child.Draw()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if moved || condition () then
            child.Update(elapsed_ms, moved)

    override this.Focusable = condition () && base.Focusable

    member this.Child = child

[<AutoOpen>]
module ConditionalExtensions =

    type Widget with
        member this.Conditional(condition: unit -> bool) = Conditional(condition, this)

[<Sealed>]
type Frame() =
    inherit StaticWidget(NodeType.None)

    member val Fill = !%Palette.DARK with get, set
    member val Border = !%Palette.LIGHT with get, set

    override this.Draw() =
        let border = this.Border()

        if border.A > 0uy then

            let r = this.Bounds.Expand Style.PADDING
            Render.rect (r.SliceL Style.PADDING) border
            Render.rect (r.SliceR Style.PADDING) border

            let r = this.Bounds.Expand(0.0f, Style.PADDING)
            Render.rect (r.SliceT Style.PADDING) border
            Render.rect (r.SliceB Style.PADDING) border

        let fill = this.Fill()

        if fill.A > 0uy then

            Render.rect base.Bounds fill

type FrameContainer(nt: NodeType) =
    inherit Container(nt)

    member val Fill = !%Palette.DARK with get, set
    member val Border = !%Palette.LIGHT with get, set

    override this.Draw() =
        let border = this.Border()

        if border.A > 0uy then

            let r = this.Bounds.Expand Style.PADDING
            Render.rect (r.SliceL Style.PADDING) border
            Render.rect (r.SliceR Style.PADDING) border

            let r = this.Bounds.Expand(0.0f, Style.PADDING)
            Render.rect (r.SliceT Style.PADDING) border
            Render.rect (r.SliceB Style.PADDING) border

        let fill = this.Fill()

        if fill.A > 0uy then

            Render.rect base.Bounds fill

        base.Draw()