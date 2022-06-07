namespace Percyqaz.Flux.UI

open System.Drawing
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input

module Alignment =
    let LEFT = 0.0f
    let CENTER = 0.5f
    let RIGHT = 1.0f

[<Sealed>]
type Text(textFunc) =
    inherit StaticWidget()

    new(text: string) = Text(K text)

    member val Align = Alignment.CENTER with get, set
    member val Color = Style.text with get, set

    override this.Draw() = Text.drawFillB(Style.baseFont, textFunc(), this.Bounds, this.Color(), this.Align)

    override this.Init(parent) =
        base.Init parent

[<Sealed>]
type Clickable(onLeftClick) =
    inherit StaticWidget()

    let mutable hover = false

    member val OnLeftClick = onLeftClick with get, set
    member val OnRightClick = ignore with get, set
    member val OnHover = ignore with get, set
    member val Floating = false with get, set

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        let oh = hover
        hover <- Mouse.hover (if this.Floating then this.Bounds else this.VisibleBounds)
        if oh && not hover then this.OnHover false
        elif not oh && hover && Mouse.moved() then this.OnHover true
        elif hover then
            if Mouse.leftClick() then this.OnLeftClick()
            if Mouse.rightClick() then this.OnRightClick()

    override this.Draw() = ()
    
[<Sealed>]
type HotkeyAction(hotkey: unit -> Bind, action) =
    inherit StaticWidget()

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        if hotkey().Tapped() then action()

    override this.Draw() = ()

[<Sealed>]
type Image(sprite: Sprite) =
    inherit StaticWidget()
    
    member val Sprite = sprite with get, set

    override this.Draw() = Draw.rect this.Bounds Color.White this.Sprite