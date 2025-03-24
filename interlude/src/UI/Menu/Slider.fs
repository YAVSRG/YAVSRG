namespace Interlude.UI

open System
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude

type Slider(setting: Setting.Bounded<float32>) =
    inherit Container(NodeType.Leaf)

    let ORIGINAL_VALUE = setting.Value
    let TEXTWIDTH = 130.0f
    let mutable dragging = false

    let mutable decimal_places = 2
    let mutable step = 0.01f

    let typed_number =
        Setting.simple ""
        |> Setting.trigger (fun v ->
            Style.key.Play()
            match Single.TryParse(v.Replace(',', '.'), Globalization.CultureInfo.InvariantCulture) with
            | true, r ->
                let lo, hi = setting.Config
                if r > hi && r / 100.0f > lo && r / 100.0f < hi then
                    setting.Set (r / 100.0f)
                else setting.Set r
            | false, _ -> ()
        )
        |> Setting.map id (fun v -> v |> Seq.filter (fun c -> c = '-' || c = '.' || c = ',' || Char.IsAsciiDigit c) |> Array.ofSeq |> String)

    let get_percent () =
        let lo, hi = setting.Config
        (setting.Value - lo) / (hi - lo)

    let set_percent (v: float32) =
        let lo, hi = setting.Config
        setting.Value <- MathF.Round((hi - lo) * v + lo, decimal_places)

    let add (v: float32) =
        setting.Value <- MathF.Round(setting.Value + v, decimal_places)
        Style.click.Play()

    override this.Init(parent) =
        this
            .Add(
                Text(fun () -> if typed_number.Value = "" then this.Format setting.Value else typed_number.Value)
                    .Align(Alignment.LEFT)
                    .Color(fun () ->
                        if typed_number.Value <> "" then Colors.text_yellow_2 else Colors.text
                    )
                    .Position(Position.SliceL TEXTWIDTH),

                MouseListener()
                    .OnLeftClick(fun () ->
                        this.Select true
                        Style.click.Play()
                        if Mouse.x() > this.Bounds.Left + TEXTWIDTH then dragging <- true
                    )
                    .OnRightClick(fun () ->
                        Style.text_close.Play()
                        typed_number.Set ""
                        setting.Value <- ORIGINAL_VALUE
                    )
                    .FocusOnHover(this)
            )
        base.Init parent

    member this.Step
        with get () : float32 = step
        and set (value: float32) =
            step <- value
            decimal_places <- max 0 (int (MathF.Ceiling(- MathF.Log10(step))))

    member val Format : float32 -> string = (fun x -> x.ToString("n" + decimal_places.ToString())) with get, set

    static member Percent(setting) =
        Slider(
            setting,
            Format = (fun x ->
                let percent = x * 100.0f |> round
                // handles what the F# foundation couldn't: -0 floating point value
                if percent = 0.0f then sprintf "%.0f%%" 0.0f
                else sprintf "%.0f%%" percent
            )
        )

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.OnSelected(by_mouse: bool) =
        base.OnSelected by_mouse
        Input.listen_to_text(typed_number, false, ignore)

    override this.OnDeselected(by_mouse: bool) =
        base.OnDeselected by_mouse
        Input.remove_listener()
        typed_number.Set ""

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

            if (%%"left").PressedOrRepeated() then
                add (-step)
            elif (%%"right").PressedOrRepeated() then
                add (step)
            elif (%%"up").PressedOrRepeated() then
                add (step * 5.0f)
            elif (%%"down").PressedOrRepeated() then
                add (-step * 5.0f)
            elif (%%"select").Pressed() then
                typed_number.Set ""

        if this.Focused && (%%"undo").Pressed() then
            Style.text_close.Play()
            typed_number.Set ""
            setting.Value <- ORIGINAL_VALUE

    abstract member DrawBar : Rect * float32 -> unit
    default this.DrawBar (bounds: Rect, percent: float32) =
        let cursor_x = bounds.Left + bounds.Width * percent

        Render.rect_edges cursor_x bounds.Top bounds.Right bounds.Bottom
            (if this.Selected then
                 Colors.pink_shadow.O3
             else
                 Colors.grey_2.O2)

        Render.rect_edges bounds.Left bounds.Top cursor_x bounds.Bottom
            (if this.Selected then Colors.pink_accent else Colors.grey_2)

    override this.Draw() =
        this.DrawBar(this.Bounds.ShrinkL(TEXTWIDTH).ShrinkY(10.0f), get_percent () |> min 1.0f |> max 0.0f)
        base.Draw()