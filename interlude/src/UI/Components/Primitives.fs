namespace Interlude.UI.Components

open System
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude.Common
open Interlude
open Interlude.Utils
open Interlude.UI

type TextBox (textFunc, color, just) =
    inherit Widget1()

    new(textFunc, scolor, just) = TextBox(textFunc, (fun () -> scolor(), Color.Transparent), just)

    override this.Draw() = 
        Text.drawFillB(Content.font, textFunc(), this.Bounds, color(), just)
        base.Draw()

type Clickable (onClick, onHover) =
    inherit Widget1()

    let mutable hover = false

    member val Float = false with get, set

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        let oh = hover
        hover <- Mouse.hover (if this.Float then this.Bounds else this.VisibleBounds)
        if oh && not hover then onHover false
        elif not oh && hover && Mouse.moved() then onHover true
        elif hover && Mouse.leftClick() then onClick()

type Bindable (bind: Hotkey, onPress) =
    inherit Widget1()

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        if (!|bind).Tapped() then onPress()
        
        
type Frame (fillColor: unit -> Color, frameColor: unit -> Color, fill, frame) =
    inherit Widget1()
        
    let BORDERWIDTH = 5.0f
        
    new() = Frame ((fun () -> Style.color(200, 0.5f, 0.3f)), (fun () -> Style.color(80, 0.5f, 0.0f)), true, true)
    new((), frame) = Frame (K Color.Transparent, K frame, false, true)
    new((), frame) = Frame (K Color.Transparent, frame, false, true)
    new(fill, ()) = Frame (K fill, K Color.Transparent, true, false)
    new(fill, ()) = Frame (fill, K Color.Transparent, true, false)
    new(fill, frame) = Frame (K fill, K frame, true, true)
    new(fill, frame) = Frame (fill, frame, true, true)
        
    override this.Draw() =
        if frame then
            let c = frameColor()
            let r = this.Bounds.Expand BORDERWIDTH
            Draw.rect (r.SliceLeft BORDERWIDTH) c
            Draw.rect (r.SliceRight BORDERWIDTH) c
            let r = this.Bounds.Expand(0.0f, BORDERWIDTH)
            Draw.rect (r.SliceTop BORDERWIDTH) c
            Draw.rect (r.SliceBottom BORDERWIDTH) c
        
        if fill then Draw.rect base.Bounds (fillColor())
        base.Draw()
        
    static member Create(w: Widget1) =
        Frame() |-+ w