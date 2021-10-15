namespace Interlude.UI.Components.Selection.Buttons

open System
open System.Drawing
open Prelude.Common
open Interlude.Utils
open Interlude.Graphics
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Components.Selection

type BigButton(label, icon, onClick) as this =
    inherit Selectable()
    do
        this.Add(Frame((fun () -> Style.accentShade(180, 0.9f, 0.0f)), (fun () -> if this.Hover then Color.White else Color.Transparent)))
        this.Add(TextBox(K label, K (Color.White, Color.Black), 0.5f) |> positionWidget(0.0f, 0.0f, 0.0f, 0.6f, 0.0f, 1.0f, 0.0f, 0.8f))
        this.Add(TextBox(K ([|"❖";"✎";"♛";"⌨";"⚒"|].[icon]), K (Color.White, Color.Black), 0.5f) |> positionWidget(0.0f, 0.0f, 0.0f, 0.05f, 0.0f, 1.0f, 0.0f, 0.7f))
        this.Add(Clickable((fun () -> this.Selected <- true), fun b -> if b then this.Hover <- true))

    override this.OnSelect() =
        this.Selected <- false
        onClick()

type LittleButton(label, onClick) as this =
    inherit Selectable()
    do
        this.Add(Frame(Color.FromArgb(80, 255, 255, 255), ()))
        this.Add(TextBox(label, (fun () -> ((if this.Hover then Style.accentShade(255, 1.0f, 0.7f) else Color.White), Color.Black)), 0.5f))
        this.Add(Clickable((fun () -> this.Selected <- true), fun b -> if b then this.Hover <- true))
    override this.OnSelect() =
        this.Selected <- false
        onClick()
    static member FromEnum<'T when 'T: enum<int>>(label: string, setting: Setting<'T>, onClick) =
        let names = Enum.GetNames(typeof<'T>)
        let values = Enum.GetValues(typeof<'T>) :?> 'T array
        let mutable i = array.IndexOf(values, setting.Value)
        LittleButton((fun () -> sprintf "%s: %s" label names.[i]),
            (fun () -> i <- (i + 1) % values.Length; setting.Value <- values.[i]; onClick()))

type CardButton(title, subtitle, highlight, onClick, colorFunc) as this =
    inherit Selectable()

    do
        if subtitle <> "" then
            TextBox(K title, K (Color.White, Color.Black), 0.0f)
            |> positionWidget(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.6f)
            |> this.Add

            TextBox(K subtitle, K (Color.White, Color.Black), 0.0f)
            |> positionWidget(0.0f, 0.0f, 0.0f, 0.6f, 0.0f, 1.0f, 0.0f, 1.0f)
            |> this.Add
        else TextBox(K title, K (Color.White, Color.Black), 0.0f) |> this.Add

        Clickable(
            (fun () -> if this.SParent.Value.Selected then this.Selected <- true),
            (fun b -> if b && this.SParent.Value.Selected then this.Hover <- true))
        |> this.Add

    new(title, subtitle, highlight, onClick) = CardButton(title, subtitle, highlight, onClick, fun () -> Style.accentShade(255, 1.0f, 0.0f))

    override this.Draw() =
        let hi = colorFunc()
        let lo = Color.FromArgb(100, hi)
        let e = highlight()
        Draw.quad (Quad.ofRect this.Bounds)
            (struct((if this.Hover then hi else lo), (if e then hi else lo), (if e then hi else lo), if this.Hover then hi else lo))
            Sprite.DefaultQuad
        base.Draw()

    override this.OnSelect() =
        base.OnSelect()
        onClick()
        this.Selected <- false