namespace Interlude.UI.Components

open System
open OpenTK.Windowing.GraphicsLibraryFramework
open Prelude.Common
open Interlude
open Interlude.Options
open Interlude.Utils
open Interlude.Input
open Interlude.Graphics
open Interlude.UI

[<AutoOpen>]
module Position =
    
    let positionWidgetA(l, t, r, b) (w: Widget) : Widget =
        w.Reposition(l, t, r, b)
        w

type TextBox(textFunc, color, just) =
    inherit Widget()

    new(textFunc, scolor, just) = TextBox(textFunc, (fun () -> scolor(), Color.Transparent), just)

    override this.Draw() = 
        Text.drawFillB(Content.font, textFunc(), this.Bounds, color(), just)
        base.Draw()

type Clickable (onClick, onHover) =
    inherit Widget()

    let mutable hover = false

    member val Float = false with get, set

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        let oh = hover
        hover <- Mouse.Hover (if this.Float then this.Bounds else this.VisibleBounds)
        if oh && not hover then onHover false
        elif not oh && hover && Mouse.Moved() then onHover true
        elif hover && Mouse.Click MouseButton.Left then onClick()

type Bindable (bind: Hotkey, onPress) =
    inherit Widget()

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        if (!|bind).Tapped() then onPress()
        
        
type Frame(fillColor: unit -> Color, frameColor: unit -> Color, fill, frame) =
    inherit Widget()
        
    let BORDERWIDTH = 5.0f
        
    new() = Frame ((fun () -> Style.accentShade(200, 0.5f, 0.3f)), (fun () -> Style.accentShade(80, 0.5f, 0.0f)), true, true)
    new((), frame) = Frame (K Color.Transparent, K frame, false, true)
    new((), frame) = Frame (K Color.Transparent, frame, false, true)
    new(fill, ()) = Frame (K fill, K Color.Transparent, true, false)
    new(fill, ()) = Frame (fill, K Color.Transparent, true, false)
    new(fill, frame) = Frame (K fill, K frame, true, true)
    new(fill, frame) = Frame (fill, frame, true, true)
        
    override this.Draw() =
        if frame then
            let c = frameColor()
            let r = Rect.expand(BORDERWIDTH, BORDERWIDTH) this.Bounds
            Draw.rect (Rect.sliceLeft BORDERWIDTH r) c Sprite.Default
            Draw.rect (Rect.sliceRight BORDERWIDTH r) c Sprite.Default
            let r = Rect.expand(0.0f, BORDERWIDTH) this.Bounds
            Draw.rect (Rect.sliceTop BORDERWIDTH r) c Sprite.Default
            Draw.rect (Rect.sliceBottom BORDERWIDTH r) c Sprite.Default
        
        if fill then Draw.rect base.Bounds (fillColor()) Sprite.Default
        base.Draw()
        
    static member Create(w: Widget) =
        let f = Frame()
        f.Add(w)
        f