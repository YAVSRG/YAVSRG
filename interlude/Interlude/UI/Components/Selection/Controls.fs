namespace Interlude.UI.Components.Selection.Controls

open System
open System.Drawing
open OpenTK
open OpenTK.Windowing.GraphicsLibraryFramework
open Prelude.Common
open Interlude
open Interlude.Utils
open Interlude.Graphics
open Interlude.Input
open Interlude.UI
open Interlude.UI.Animation
open Interlude.UI.Components
open Interlude.UI.Components.Selection
open Interlude.UI.Components.Selection.Containers


type Selector(items: string array, setting: Setting<int>) as this =
    inherit NavigateSelectable()
    let fd() = setting.Value <- (setting.Value + 1) % items.Length
    let bk() = setting.Value <- (setting.Value + items.Length - 1) % items.Length
    do
        this.Add(new TextBox((fun () -> items.[setting.Value]), K (Color.White, Color.Black), 0.0f))
        this.Reposition(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 100.0f, 0.0f)
        this.Add(new Clickable((fun () -> (if not this.Selected then this.Selected <- true); fd()), fun b -> if b then this.Hover <- true))

    override this.Left() = bk()
    override this.Down() = bk()
    override this.Up() = fd()
    override this.Right() = fd()

    static member FromArray<'T>(names: string array, values: 'T array, setting: Setting<'T>) =
        new Selector(names,
            Setting.map
                (fun e -> Array.IndexOf(values, e))
                (fun i -> values.[i])
                setting)

    static member FromEnum<'T>(setting: Setting<'T>) =
        let names = Enum.GetNames(typeof<'T>)
        let values = Enum.GetValues(typeof<'T>) :?> 'T array
        Selector.FromArray(names, values, setting)

    static member FromBool(setting: Setting<bool>) =
        new Selector([|"◇" ; "◆"|],
            Setting.map
                (fun b -> if b then 1 else 0)
                (fun i -> i > 0)
                setting)

type Slider<'T>(setting: Setting.Bounded<'T>, incr: float32) as this =
    inherit NavigateSelectable()
    let TEXTWIDTH = 130.0f
    let color = AnimationFade 0.5f
    let mutable dragging = false
        
    let getPercent (setting: Setting.Bounded<'T>) =
        let (Setting.Bounds (lo, hi)) = setting.Config
        let (lo, hi) = (Convert.ToSingle lo, Convert.ToSingle hi)
        let value = Convert.ToSingle setting.Value
        (value - lo) / (hi - lo)

    let setPercent (v: float32) (setting: Setting.Bounded<'T>) =
        let (Setting.Bounds (lo, hi)) = setting.Config
        let (lo, hi) = (Convert.ToSingle lo, Convert.ToSingle hi)
        setting.Value <- Convert.ChangeType((hi - lo) * v + lo, typeof<'T>) :?> 'T

    let chPercent v = setPercent (getPercent setting + v) setting
    do
        this.Animation.Add color
        this.Add(new TextBox((fun () -> setting.Value.ToString()), K (Color.White, Color.Black), 0.0f) |> positionWidget(0.0f, 0.0f, 0.0f, 0.0f, TEXTWIDTH, 0.0f, 0.0f, 1.0f))
        this.Reposition(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 100.0f, 0.0f)
        this.Add(new Clickable((fun () -> this.Selected <- true; dragging <- true), fun b -> color.Target <- if b then this.Hover <- true; 0.8f else 0.5f))

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        let struct (l, t, r, b) = Rect.trimLeft TEXTWIDTH this.Bounds
        if this.Selected then
            if (Mouse.Held(MouseButton.Left) && dragging) then
                let amt = (Mouse.X() - l) / (r - l)
                setPercent amt setting
            else dragging <- false

    override this.Left() = chPercent(-incr)
    override this.Up() = chPercent(incr * 10.0f)
    override this.Right() = chPercent(incr)
    override this.Down() = chPercent(-incr * 10.0f)

    override this.Draw() =
        let v = getPercent setting
        let struct (l, t, r, b) = Rect.trimLeft TEXTWIDTH this.Bounds
        let cursor = Rect.create (l + (r - l) * v) t (l + (r - l) * v) b |> Rect.expand(10.0f, -10.0f)
        let m = (b + t) * 0.5f
        Draw.rect (Rect.create l (m - 10.0f) r (m + 10.0f)) (Style.accentShade(255, 1.0f, 0.0f)) Sprite.Default
        Draw.rect cursor (Style.accentShade(255, 1.0f, color.Value)) Sprite.Default
        base.Draw()

type TextField(setting: Setting<string>) as this =
    inherit Selectable()
    let color = AnimationFade 0.5f
    do
        this.Animation.Add color
        this.Add(new TextBox(setting.Get, (fun () -> Style.accentShade(int (color.Value * 255.0f), 1.0f, color.Value), Color.Black), 0.0f))
        this.Add(new Clickable((fun () -> if not this.Selected then this.Selected <- true), fun b -> if b then this.Hover <- true))

    override this.OnSelect() =
        base.OnSelect()
        color.Target <- 1.0f
        Input.setInputMethod (setting, fun () -> this.Selected <- false)

    override this.OnDeselect() =
        base.OnDeselect()
        color.Target <- 0.5f

type ColorPicker(color: Setting<byte>) as this =
    inherit NavigateSelectable()
    let sprite = Content.getTexture "note"
    let n = byte sprite.Rows
    let fd() = Setting.app (fun x -> (x + n - 1uy) % n) color
    let bk() = Setting.app (fun x -> (x + 1uy) % n) color
    do this.Add(new Clickable((fun () -> (if not this.Selected then this.Selected <- true); fd ()), fun b -> if b then this.Hover <- true))

    override this.Draw() =
        base.Draw()
        if this.Selected then Draw.rect this.Bounds (Style.accentShade(180, 1.0f, 0.5f)) Sprite.Default
        elif this.Hover then Draw.rect this.Bounds (Style.accentShade(120, 1.0f, 0.8f)) Sprite.Default
        Draw.quad (Quad.ofRect this.Bounds) (Quad.colorOf Color.White) (Sprite.gridUV (3, int color.Value) sprite)

    override this.Left() = bk()
    override this.Up() = fd()
    override this.Right() = fd()
    override this.Down() = bk()

type KeyBinder(setting: Setting<Bind>, allowModifiers) as this =
    inherit Selectable()
    do
        this.Add(new TextBox((fun () -> setting.Value.ToString()), (fun () -> (if this.Selected then Style.accentShade(255, 1.0f, 0.0f) else Color.White), Color.Black), 0.5f) |> positionWidgetA(0.0f, 40.0f, 0.0f, -40.0f))
        this.Add(new Clickable((fun () -> if not this.Selected then this.Selected <- true), fun b -> if b then this.Hover <- true))

    override this.Draw() =
        if this.Selected then Draw.rect this.Bounds (Style.accentShade(180, 1.0f, 0.5f)) Sprite.Default
        elif this.Hover then Draw.rect this.Bounds (Style.accentShade(120, 1.0f, 0.8f)) Sprite.Default
        Draw.rect (Rect.expand(0.0f, -40.0f) this.Bounds) (Style.accentShade(127, 0.8f, 0.0f)) Sprite.Default
        base.Draw()

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        if this.Selected then
            match Input.consumeAny InputEvType.Press with
            | ValueNone -> ()
            | ValueSome b ->
                match b with
                | Key (k, (ctrl, _, shift)) ->
                    if k = Keys.Escape then if allowModifiers then setting.Value <- Dummy
                    elif allowModifiers then setting.Value <- Key (k, (ctrl, false, shift))
                    else setting.Value <- Key (k, (false, false, false))
                    this.Selected <- false
                | _ -> ()