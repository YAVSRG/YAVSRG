namespace Interlude.UI.Components

open System
open System.Drawing
open Prelude.Common
open Interlude
open Interlude.Utils
open Interlude.UI
open Interlude.Input
open Interlude.Graphics
open Interlude.UI.Animation

type Button(onClick, label, bind: Setting<Bind>) as this =
    inherit Widget()

    let color = AnimationFade 0.3f

    do
        this.Animation.Add color
        this.Add(new Clickable(onClick, fun b -> color.Target <- if b then 0.7f else 0.3f))
        this.Add(new Bindable(bind, onClick))

    new(onClick, label) = Button(onClick, label, Bind.DummyBind)

    override this.Draw() =
        Draw.rect this.Bounds (Style.accentShade(80, 0.5f, color.Value)) Sprite.Default
        Draw.rect (Rect.sliceBottom 10.0f this.Bounds) (Style.accentShade(255, 1.0f, color.Value)) Sprite.Default
        Text.drawFillB(Content.font(), label, Rect.trimBottom 10.0f this.Bounds, (Style.accentShade(255, 1.0f, color.Value), Style.accentShade(255, 0.4f, color.Value)), 0.5f)

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

    let DropdownButton(label, v, onClick) =
        let b = Base(onClick, Bind.DummyBind)
        TextBox(K label, Style.text, 0.5f)
        |> positionWidget (0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.4f)
        |> b.Add
        TextBox(v, Style.text, 0.5f)
        |> positionWidget (0.0f, 0.0f, 0.0f, 0.35f, 0.0f, 1.0f, 0.0f, 1.0f)
        |> b.Add
        b