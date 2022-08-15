namespace Interlude.UI.Components

open System
open OpenTK.Mathematics
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude.Common
open Interlude
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Components

type Button(onClick, labelFunc: unit -> string, bind: Hotkey) as this =
    inherit Widget1()

    let color = Animation.Fade 0.3f

    do
        this.Animation.Add color
        this.Add(new Clickable(onClick, fun b -> color.Target <- if b then 0.7f else 0.3f))
        this.Add(new Bindable(bind, onClick))

    new(onClick, labelFunc: unit -> string) = Button(onClick, labelFunc, "none")
    new(onClick, label: string) = Button(onClick, K label)
    new(onClick, label: string, bind: Hotkey) = Button(onClick, K label, bind)

    override this.Draw() =
        Draw.rect this.Bounds (Style.color(80, 0.5f, color.Value))
        Draw.rect (this.Bounds.SliceBottom 10.0f) (Style.color(255, 1.0f, color.Value))
        Text.drawFillB(Content.font, labelFunc(), this.Bounds.TrimBottom 10.0f, (Style.color(255, 1.0f, color.Value), Style.color(255, 0.4f, color.Value)), 0.5f)

type StylishButton(onClick, labelFunc: unit -> string, colorFunc, bind: Hotkey) as this =
    inherit StaticContainer(NodeType.Button onClick)
    
    let color = Animation.Fade 0.3f

    do
        this
        |+ Clickable.Focus this
        |* HotkeyAction(bind, onClick) // todo: create this at init-time and make hotkey optional
    
    // todo: remove this and make hotkey optional
    new(onClick, labelFunc, colorFunc) = StylishButton(onClick, labelFunc, colorFunc, "none")

    member val TiltLeft = true with get, set
    member val TiltRight = true with get, set

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        color.Update elapsedTime

    override this.Draw() =
        let h = this.Bounds.Height
        Draw.quad
            ( Quad.create
                <| Vector2(this.Bounds.Left, this.Bounds.Top)
                <| Vector2(this.Bounds.Right + (if this.TiltRight then h * 0.5f else 0.0f), this.Bounds.Top)
                <| Vector2(this.Bounds.Right, this.Bounds.Bottom)
                <| Vector2(this.Bounds.Left - (if this.TiltLeft then h * 0.5f else 0.0f), this.Bounds.Bottom)
            ) (colorFunc () |> Quad.colorOf)
            Sprite.DefaultQuad
        Text.drawFillB(Content.font, labelFunc(), this.Bounds, (Style.highlightF 255 color.Value, Style.color(255, 0.4f, color.Value)), 0.5f)
        base.Draw()

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
    
    type Base(onClick, bind: Hotkey) as this =
        inherit Widget1()

        let mutable hover = false
        do
            this.Add(Frame(Style.main 100, fun () -> if hover then Color.White else Style.highlightL 100 ()))
            this.Add(new Clickable(onClick, fun b -> hover <- b))
            this.Add(new Bindable(bind, onClick))

    let Basic(label, onClick, bind: Hotkey) =
        let b = Base(onClick, bind)
        TextBox(K label, Style.text, 0.5f) |> b.Add
        b