namespace Interlude.UI

open System
open Interlude
open Interlude.Render
open Interlude.UI.Animation
open OpenTK

module Components =
    open Interlude.Input
    
    let positionWidgetA(l, t, r, b) (w: Widget) : Widget =
        w.Reposition(l, t, r, b)
        w

    let positionWidget(l, la, t, ta, r, ra, b, ba) (w: Widget) : Widget =
        w.Reposition(l, la, t, ta, r, ra, b, ba)
        w
    
    type FilledRect(c) =
        inherit Widget()
        override this.Draw() = Draw.rect base.Bounds c Sprite.Default

    type TextBox(textFunc, color, just) =
        inherit Widget()
        override this.Draw() = 
            let struct (l, t, _, _) = base.Bounds
            Text.drawFill(Themes.font(), textFunc(), this.Bounds, color(), just)

    type Clickable(onClick, onHover) =
        inherit Widget()

        let mutable hover = false

        override this.Update(time, (l, t, r, b)) =
            base.Update(time, (l, t, r, b))
            let x, y = Mouse.X(), Mouse.Y()
            let oh = hover
            hover <- x > l && x < r && y > t && y < b
            if oh <> hover then onHover(hover)
            if hover && Mouse.Click(Input.MouseButton.Left) then onClick()

    type Button(onClick, label, sprite) as this =
        inherit Widget()

        let color = AnimationFade(0.0f)

        do
            this.Animation.Add(color)
            this.Add(new Clickable(onClick, fun b -> color.SetTarget(if b then 0.7f else 0.0f)))

        override this.Draw() =
            Draw.rect this.Bounds (Screens.accentShade(80, 0.5f, color.Value)) Sprite.Default
            Draw.rect (Rect.sliceBottom 10.0f this.Bounds) (Screens.accentShade(255, 1.0f, color.Value)) Sprite.Default
            Text.drawFill(Themes.font(), label, Rect.trimBottom 10.0f this.Bounds, (Screens.accentShade(255, 1.0f, color.Value)), 0.5f)