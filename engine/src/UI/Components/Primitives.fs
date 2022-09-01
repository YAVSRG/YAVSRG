namespace Percyqaz.Flux.UI

open System.Drawing
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input

module Alignment =
    let LEFT = 0.0f
    let CENTER = 0.5f
    let RIGHT = 1.0f

type [<Sealed>] Dummy() =
    inherit StaticWidget(NodeType.None)
    override this.Draw() = ()

type [<Sealed>] Text(textFunc) =
    inherit StaticWidget(NodeType.None)

    new(text: string) = Text(K text)

    member val Align = Alignment.CENTER with get, set
    member val Color = Style.text with get, set

    override this.Draw() = Text.drawFillB(Style.baseFont, textFunc(), this.Bounds, this.Color(), this.Align)

    override this.Init(parent) =
        base.Init parent

type [<Sealed>] Clickable(onLeftClick) =
    inherit StaticWidget(NodeType.None)

    let mutable hover = false

    member val OnLeftClick = onLeftClick with get, set
    member val OnRightClick = ignore with get, set
    member val OnHover = ignore with get, set
    member val Floating = false with get, set

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        let was_hovering = hover
        hover <- Mouse.hover (if this.Floating then this.Bounds else this.VisibleBounds)
        if was_hovering && not hover then this.OnHover false
        elif not was_hovering && hover && ((not moved) || Mouse.moved()) then this.OnHover true
        elif hover then
            if Mouse.leftClick() then this.OnLeftClick()
            if Mouse.rightClick() then this.OnRightClick()

    override this.Draw() = ()

    static member Focus(w: Widget) =
        Clickable(w.Select, OnHover = fun b -> if b && not w.Focused then w.Focus())
    
type [<Sealed>] HotkeyAction(hotkey: Hotkey, action) =
    inherit StaticWidget(NodeType.None)

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        if (!|hotkey).Tapped() then action()

    override this.Draw() = ()

type [<Sealed>] Image(sprite: Sprite) =
    inherit StaticWidget(NodeType.None)
    
    member val Sprite = sprite with get, set

    override this.Draw() = Draw.sprite this.Bounds Color.White this.Sprite

type Frame(nodeType) =
    inherit StaticContainer(nodeType)

    member val Fill = !%Palette.DARK with get, set
    member val Border = !%Palette.LIGHT with get, set

    override this.Draw() =
        let border = this.Border()
        if border.A > 0uy then

            let r = this.Bounds.Expand Style.padding
            Draw.rect (r.SliceLeft Style.padding) border
            Draw.rect (r.SliceRight Style.padding) border

            let r = this.Bounds.Expand (0.0f, Style.padding)
            Draw.rect (r.SliceTop Style.padding) border
            Draw.rect (r.SliceBottom Style.padding) border

        let fill = this.Fill()
        if fill.A > 0uy then
            
            Draw.rect base.Bounds fill

        base.Draw()