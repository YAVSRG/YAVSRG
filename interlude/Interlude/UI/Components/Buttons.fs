namespace Interlude.UI.Components

open System
open System.Drawing
open OpenTK.Mathematics
open Prelude.Common
open Interlude
open Interlude.Utils
open Interlude.UI
open Interlude.Input
open Interlude.Graphics
open Interlude.UI.Animation

type Button(onClick, labelFunc: unit -> string, bind: Setting<Bind>) as this =
    inherit Widget()

    let color = AnimationFade 0.3f

    do
        this.Animation.Add color
        this.Add(new Clickable(onClick, fun b -> color.Target <- if b then 0.7f else 0.3f))
        this.Add(new Bindable(bind, onClick))

    new(onClick, labelFunc: unit -> string) = Button(onClick, labelFunc, Bind.DummyBind)
    new(onClick, label: string) = Button(onClick, K label)
    new(onClick, label: string, bind: Setting<Bind>) = Button(onClick, K label, bind)

    override this.Draw() =
        Draw.rect this.Bounds (Style.accentShade(80, 0.5f, color.Value)) Sprite.Default
        Draw.rect (Rect.sliceBottom 10.0f this.Bounds) (Style.accentShade(255, 1.0f, color.Value)) Sprite.Default
        Text.drawFillB(Content.font(), labelFunc(), Rect.trimBottom 10.0f this.Bounds, (Style.accentShade(255, 1.0f, color.Value), Style.accentShade(255, 0.4f, color.Value)), 0.5f)

type StylishButton(onClick, labelFunc: unit -> string, colorFunc, bind: Setting<Bind>) as this =
    inherit Widget()
    
    let color = AnimationFade 0.3f

    do
        this.Animation.Add color
        this.Add(new Clickable(onClick, fun b -> color.Target <- if b then 0.7f else 0.3f))
        this.Add(new Bindable(bind, onClick))
    
    new(onClick, labelFunc, colorFunc) = StylishButton(onClick, labelFunc, colorFunc, Bind.DummyBind)

    member val TiltLeft = true with get, set
    member val TiltRight = true with get, set

    override this.Draw() =
        let struct (left, top, right, bottom) = this.Bounds
        let h = bottom - top
        Draw.quad
            ( Quad.create
                <| Vector2(left, top)
                <| Vector2(right + (if this.TiltRight then h * 0.5f else 0.0f), top)
                <| Vector2(right, bottom)
                <| Vector2(left - (if this.TiltLeft then h * 0.5f else 0.0f), bottom)
            ) (colorFunc () |> Quad.colorOf)
            Sprite.DefaultQuad
        Text.drawFillB(Content.font(), labelFunc(), this.Bounds, (Style.highlightF 255 color.Value, Style.accentShade(255, 0.4f, color.Value)), 0.5f)

    static member FromEnum<'T when 'T: enum<int>>(label: string, setting: Setting<'T>, colorFunc) =
        let names = Enum.GetNames(typeof<'T>)
        let values = Enum.GetValues(typeof<'T>) :?> 'T array
        let mutable i = array.IndexOf(values, setting.Value)
        StylishButton(
            (fun () -> i <- (i + 1) % values.Length; setting.Value <- values.[i]), 
            (fun () -> sprintf "%s: %s" label names.[i]),
            colorFunc
        )

module CardButton =
    
    type Base(onClick, bind: Setting<Bind>) as this =
        inherit Widget()

        let mutable hover = false
        do
            this.Add(Frame(Style.main 100, fun () -> if hover then Color.White else Style.highlightL 100 ()))
            this.Add(new Clickable(onClick, fun b -> hover <- b))
            this.Add(new Bindable(bind, onClick))

    let Basic(label, onClick, bind: Setting<Bind>) =
        let b = Base(onClick, bind)
        TextBox(K label, Style.text, 0.5f) |> b.Add
        b