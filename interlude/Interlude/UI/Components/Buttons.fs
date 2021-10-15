namespace Interlude.UI.Components

open System
open System.Drawing
open OpenTK.Windowing.GraphicsLibraryFramework
open Prelude.Common
open Interlude
open Interlude.UI
open Interlude.Input
open Interlude.Graphics
open Interlude.UI.Animation

[<AutoOpen>]
module Position =
    
    let positionWidgetA(l, t, r, b) (w: Widget) : Widget =
        w.Reposition(l, t, r, b)
        w

    let positionWidget(l, la, t, ta, r, ra, b, ba) (w: Widget) : Widget =
        w.Reposition(l, la, t, ta, r, ra, b, ba)
        w

type Clickable (onClick, onHover) =
    inherit Widget()

    let mutable hover = false

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        let oh = hover
        hover <- Mouse.Hover this.VisibleBounds
        if oh && not hover then onHover false
        elif not oh && hover && Mouse.Moved() then onHover true
        elif hover && Mouse.Click MouseButton.Left then onClick()

type Bindable (bind: Setting<Bind>, onPress) =
    inherit Widget()

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        if bind.Value.Tapped() then onPress()


type Button(onClick, label, bind: Setting<Bind>) as this =
    inherit Widget()

    let color = AnimationFade 0.3f

    do
        this.Animation.Add color
        this.Add(new Clickable(onClick, (fun b -> color.Target <- if b then 0.7f else 0.3f)))
        this.Add(new Bindable(bind, onClick))

    new(onClick, label) = Button(onClick, label, Bind.DummyBind)

    override this.Draw() =
        Draw.rect this.Bounds (Style.accentShade(80, 0.5f, color.Value)) Sprite.Default
        Draw.rect (Rect.sliceBottom 10.0f this.Bounds) (Style.accentShade(255, 1.0f, color.Value)) Sprite.Default
        Text.drawFillB(Themes.font(), label, Rect.trimBottom 10.0f this.Bounds, (Style.accentShade(255, 1.0f, color.Value), Style.accentShade(255, 0.4f, color.Value)), 0.5f)