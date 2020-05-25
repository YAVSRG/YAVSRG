namespace Interlude.UI

open System
open Prelude.Common
open Interlude
open Interlude.Render
open Interlude.UI.Animation
open Interlude.Input
open OpenTK

module Components =
    
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

        override this.Update(time, bounds) =
            base.Update(time, bounds)
            let oh = hover
            hover <- Mouse.Hover(bounds)
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

    type Slider<'T when 'T : comparison>(setting : NumSetting<'T>, label) as this =
        inherit Widget()

        let color = AnimationFade(1.0f)
        let mutable dragging = false

        do
            this.Animation.Add(color)
            this.Add(new Clickable((fun () -> dragging <- true), fun b -> color.SetTarget(if b then 0.8f else 1.0f)))

        override this.Update(time, (l, t, r, b)) =
            base.Update(time, (l, t, r, b))
            if (Mouse.pressed(Input.MouseButton.Left) && dragging) then
                let amt = (Mouse.X() - l) / (r - l)
                setting.SetPercent(amt)
            else
                dragging <- false

        override this.Draw() =
            let v = setting.GetPercent()
            let struct (l, t, r, b) = this.Bounds
            let cursor = Rect.create (l + (r - l) * v) (b - 20.0f) (l + (r - l) * v) (b - 10.0f)
            Text.drawFill(Themes.font(), sprintf "%s: %s" label <| setting.Get().ToString(), Rect.trimBottom 30.0f this.Bounds, Color.White, 0.5f)
            Draw.rect (this.Bounds |> Rect.sliceBottom 30.0f |> Rect.expand(5.0f, -5.0f)) Color.Black Sprite.Default
            Draw.rect (this.Bounds |> Rect.sliceBottom 30.0f |> Rect.expand(0.0f, -10.0f)) (Screens.accentShade(255, 1.0f, v)) Sprite.Default
            Draw.rect (cursor |> Rect.expand(15.0f, 15.0f)) Color.Black Sprite.Default
            Draw.rect (cursor |> Rect.expand(10.0f, 10.0f)) (Screens.accentShade(255, 1.0f, color.Value)) Sprite.Default

    type FlowContainer(?spacingX: float32, ?spacingY: float32) =
        inherit Widget()
        let spacingX, spacingY = (defaultArg spacingX 10.0f, defaultArg spacingY 5.0f)
        let mutable contentSize = 0.0f
        let mutable scrollPos = 0.0f

        member private this.FlowContent(instant) =
            let width = Rect.width this.Bounds
            let height = Rect.height this.Bounds
            let f (anchor : AnchorPoint) = if instant then anchor.RepositionRelative else anchor.MoveRelative
            let x = 0.0f
            let y = -scrollPos
            contentSize <-
                this.Children
                |> Seq.fold (fun (x, y) w ->
                    let (l, t, r, b) = w.Position
                    let cwidth = r.Position(0.0f, width) - l.Position(0.0f, width)
                    let cheight = b.Position(0.0f, height) - t.Position(0.0f, height)
                    let x = x + cwidth + spacingX
                    let x, y = if x > width then cwidth, y + cheight + spacingY else x, y
                    f l (0.0f, width, x - cwidth - spacingX); f t (0.0f, height, y - cheight)
                    f r (0.0f, width, x - spacingX); f b (0.0f, height, y)
                    (x + spacingY, y)
                    ) (0.0f, -scrollPos)
                |> snd

        override this.Update(time, bounds) =
            if (this.Initialised) then
                this.FlowContent(false)
                base.Update(time, bounds)
            else
                base.Update(time,bounds)
                this.FlowContent(true)
                if Mouse.Hover(bounds) then scrollPos <- Math.Clamp(scrollPos - (Mouse.Scroll() |> float32) * 100.0f, 0.0f, contentSize)
