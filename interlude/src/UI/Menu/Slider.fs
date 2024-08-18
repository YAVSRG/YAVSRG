namespace Interlude.UI

open System
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude

type Slider(setting: Setting.Bounded<float32>) as this =
    inherit Container(NodeType.Leaf)

    let TEXTWIDTH = 130.0f
    let mutable dragging = false

    let mutable decimal_places = 2
    let mutable step = 0.01f

    let get_percent () =
        let (Setting.Bounds(lo, hi)) = setting.Config
        (setting.Value - lo) / (hi - lo)

    let set_percent (v: float32) =
        let (Setting.Bounds(lo, hi)) = setting.Config
        setting.Value <- MathF.Round((hi - lo) * v + lo, decimal_places)

    let add (v) =
        setting.Value <- MathF.Round(setting.Value + v, decimal_places)
        Style.click.Play()

    do
        this
        |+ Text(
            (fun () -> this.Format setting.Value),
            Align = Alignment.LEFT,
            Position =
                { Position.DEFAULT with
                    Right = 0.0f %+ TEXTWIDTH
                }
        )
        |* Clickable(
            (fun () ->
                this.Select true
                Style.click.Play()
                dragging <- true
            ),
            OnHover =
                (fun b ->
                    if b && not this.Focused then
                        this.Focus true
                    elif not b && this.FocusedByMouse then
                        Selection.up true
                )
        )

    member this.Step
        with get () = step
        and set (value) =
            step <- value
            decimal_places <- max 0 (int (MathF.Ceiling(- MathF.Log10(step))))

    member val Format : float32 -> string = (fun x -> x.ToString("n" + decimal_places.ToString())) with get, set

    static member Percent(setting) =
        Slider(setting, Format = (fun x -> sprintf "%.0f%%" (x * 100.0f)))

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        let bounds = this.Bounds.ShrinkL TEXTWIDTH

        if dragging && Mouse.held Mouse.LEFT then
            let l, r = bounds.Left, bounds.Right
            let amt = (Mouse.x () - l) / (r - l)
            set_percent amt
        elif dragging then
            Style.click.Play()
            dragging <- false

        if this.Selected || Mouse.hover this.Bounds then
            let s = Mouse.scroll ()

            if s > 0.0f then
                setting.Value <- setting.Value + step
            elif s < 0.0f then
                setting.Value <- setting.Value - step

        if this.Selected then

            if (%%"left").Tapped() then
                add (-step)
            elif (%%"right").Tapped() then
                add (step)
            elif (%%"up").Tapped() then
                add (step * 5.0f)
            elif (%%"down").Tapped() then
                add (-step * 5.0f)

    override this.Draw() =
        let v = get_percent ()
        let bounds = this.Bounds.ShrinkL TEXTWIDTH

        let cursor_x = bounds.Left + bounds.Width * v

        Draw.rect
            (Rect.Create(cursor_x, (bounds.Top + 10.0f), bounds.Right, (bounds.Bottom - 10.0f)))
            (if this.Selected then
                 Colors.pink_shadow.O3
             else
                 Colors.grey_2.O2)

        Draw.rect
            (Rect.Create(bounds.Left, (bounds.Top + 10.0f), cursor_x, (bounds.Bottom - 10.0f)))
            (if this.Selected then Colors.pink_accent else Colors.grey_2)

        base.Draw()