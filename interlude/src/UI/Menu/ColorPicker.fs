namespace Interlude.UI

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude

type ColorPicker(s: Setting<Color>, allow_alpha: bool) as this =
    inherit Container(NodeType.Container(fun _ -> Some this.HexEditor))

    let HEX_EDITOR_HEIGHT = PRETTYHEIGHT * 0.6f

    let (H, S, V) = s.Value.ToHsv()
    let mutable H = H
    let mutable S = S
    let mutable V = V
    let mutable A = if allow_alpha then float32 s.Value.A / 255.0f else 1.0f

    let mutable dragging_sv = false
    let mutable dragging_h = false
    let mutable dragging_a = false

    let hex =
        Setting.simple (s.Value.ToHex())
        |> Setting.trigger (fun color ->
            try
                s.Value <- Color.FromHex color
                let (h, s, v) = s.Value.ToHsv()
                H <- h
                S <- s
                V <- v
            with _ -> ()
        )

    let hex_editor =
        { new TextEntry(hex, "none", false, Position = Position.ShrinkL(50.0f).SliceT HEX_EDITOR_HEIGHT) with
            override this.OnDeselected(by_mouse: bool) =
                base.OnDeselected by_mouse
                hex.Value <- s.Value.ToHex()
        }

    let s = Setting.trigger (fun (c: Color) -> hex.Value <- c.ToHex()) s

    do this.Add hex_editor

    member private this.HexEditor = hex_editor

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Draw() =
        base.Draw()

        let preview = this.Bounds.SliceT(HEX_EDITOR_HEIGHT).SliceL(50.0f).Shrink(5.0f)

        let saturation_value_picker =
            this.Bounds.ShrinkT(HEX_EDITOR_HEIGHT).SliceL(200.0f).Shrink(5.0f)

        let hue_picker =
            this.Bounds
                .ShrinkT(HEX_EDITOR_HEIGHT)
                .SliceL(230.0f)
                .ShrinkL(200.0f)
                .Shrink(5.0f)

        let alpha_picker =
            this.Bounds
                .ShrinkT(HEX_EDITOR_HEIGHT)
                .SliceL(260.0f)
                .ShrinkL(230.0f)
                .Shrink(5.0f)

        Draw.rect preview s.Value

        Draw.quad
            (saturation_value_picker.AsQuad)
            { TopLeft = Color.White; TopRight = Color.FromHsv(H, 1.0f, 1.0f); BottomRight = Color.Black; BottomLeft = Color.Black }

        let x = saturation_value_picker.Left + S * saturation_value_picker.Width
        let y = saturation_value_picker.Bottom - V * saturation_value_picker.Height
        Draw.rect (Rect.Create(x - 2.5f, y - 2.5f, x + 2.5f, y + 2.5f)) Color.White

        let h = hue_picker.Height / 6.0f

        for i = 0 to 5 do
            let a = Color.FromHsv(float32 i / 6.0f, 1.0f, 1.0f)
            let b = Color.FromHsv((float32 i + 1.0f) / 6.0f, 1.0f, 1.0f)

            Draw.quad
                (Rect.Box(hue_picker.Left, hue_picker.Top + h * float32 i, hue_picker.Width, h))
                    .AsQuad
                (Quad.gradient_top_to_bottom a b)

        Draw.rect
            (Rect.Box(hue_picker.Left, hue_picker.Top + H * (hue_picker.Height - 5.0f), hue_picker.Width, 5.0f))
            Color.White

        if allow_alpha then
            Draw.quad
                alpha_picker.AsQuad
                (Quad.gradient_top_to_bottom (s.Value.O4a 0) s.Value)

            Draw.rect
                (Rect.Box(
                    alpha_picker.Left,
                    alpha_picker.Top + A * (alpha_picker.Height - 5.0f),
                    alpha_picker.Width,
                    5.0f
                ))
                Color.White

    override this.Update(elapsed_ms, moved) =

        base.Update(elapsed_ms, moved)

        let saturation_value_picker =
            this.Bounds.ShrinkT(HEX_EDITOR_HEIGHT).SliceL(200.0f).Shrink(5.0f)

        let hue_picker =
            this.Bounds
                .ShrinkT(HEX_EDITOR_HEIGHT)
                .SliceL(230.0f)
                .ShrinkL(200.0f)
                .Shrink(5.0f)

        let alpha_picker =
            this.Bounds
                .ShrinkT(HEX_EDITOR_HEIGHT)
                .SliceL(260.0f)
                .ShrinkL(230.0f)
                .Shrink(5.0f)

        if Mouse.hover saturation_value_picker && Mouse.left_click() then
            dragging_sv <- true

        if dragging_sv then
            let x, y = Mouse.pos ()
            S <- (x - saturation_value_picker.Left) / saturation_value_picker.Width |> min 1.0f |> max 0.0f
            V <- 1.0f - (y - saturation_value_picker.Top) / saturation_value_picker.Height |> min 1.0f |> max 0.0f
            s.Value <- Color.FromArgb(int (A * 255.0f), Color.FromHsv(H, S, V))
            if not (Mouse.held Mouse.LEFT) then dragging_sv <- false

        elif Mouse.hover hue_picker && Mouse.left_click() then
            dragging_h <- true

        if dragging_h then
            let y = Mouse.y ()
            H <- (y - hue_picker.Top) / hue_picker.Height |> min 1.0f |> max 0.0f
            s.Value <- Color.FromArgb(int (A * 255.0f), Color.FromHsv(H, S, V))
            if not (Mouse.held Mouse.LEFT) then dragging_h <- false

        elif allow_alpha && Mouse.hover alpha_picker && Mouse.left_click() then
            dragging_a <- true

        if dragging_a then
            let y = Mouse.y ()
            A <- (y - alpha_picker.Top) / alpha_picker.Height |> min 1.0f |> max 0.0f
            s.Value <- Color.FromArgb(int (A * 255.0f), Color.FromHsv(H, S, V))
            if not (Mouse.held Mouse.LEFT) then dragging_a <- false
