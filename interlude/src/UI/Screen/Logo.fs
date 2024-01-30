namespace Interlude.UI

open System
open OpenTK.Mathematics
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Audio
open Percyqaz.Flux.UI
open Interlude.Content

module Logo =

    type private State =
        | Centre
        | Menu
        | Hidden

    let mutable private state = Centre

    let GRADIENT = 1.61803f
    let PADDING = 10f
    let Y_PADDING_FOR_GRADIENT = PADDING * MathF.Sqrt(GRADIENT * GRADIENT + 1.0f)
    let X_PADDING_FOR_GRADIENT = (Y_PADDING_FOR_GRADIENT + PADDING) / GRADIENT
    let X_PADDING_FOR_GRADIENT_INNER = (Y_PADDING_FOR_GRADIENT - PADDING) / GRADIENT

    let BOBBING_INTENSITY = 0.01f
    let BREATHING_INTENSITY = 0.01f

    let TRIANGLE_HEIGHT = 0.4f * GRADIENT

    let LOWER_Y_THICKNESS =
        0.85f - TRIANGLE_HEIGHT - BREATHING_INTENSITY - (3.0f * PADDING / 800.0f)

    let LOWER_X_THICKNESS = LOWER_Y_THICKNESS / GRADIENT

    type Display() =
        inherit DynamicContainer(NodeType.None)

        let counter = Animation.Counter(10000000.0)

        override this.Draw() =
            if state = Hidden then
                ()
            else
                base.Draw()
                let w = this.Bounds.Width

                let breathe_1 =
                    float32 (Math.Sin(counter.Time / 3500.0 * Math.PI)) * BOBBING_INTENSITY * w

                let breathe_2 =
                    (float32 (Math.Cos(counter.Time / 8000.0 * Math.PI)) * BREATHING_INTENSITY
                     + 2.0f * BREATHING_INTENSITY)
                    * w

                let breathe_bounds = this.Bounds.Translate(0.0f, breathe_1)

                let {
                        Rect.Left = l
                        Top = t
                        Right = r
                        Bottom = b
                    } =
                    breathe_bounds

                if r > 2.0f then
                    /// DARK BLUE BACKDROP

                    let UPPER_TRIANGLE_1 = Vector2(l + 0.1f * w, t + 0.1f * w)
                    let UPPER_TRIANGLE_2 = Vector2(r - 0.1f * w, t + 0.1f * w)
                    let UPPER_TRIANGLE_3 = Vector2(l + 0.5f * w, t + (0.1f + TRIANGLE_HEIGHT) * w)

                    Draw.untextured_quad
                        (Quad.create
                            (UPPER_TRIANGLE_1 + Vector2(-X_PADDING_FOR_GRADIENT, -PADDING))
                            (UPPER_TRIANGLE_2 + Vector2(X_PADDING_FOR_GRADIENT, -PADDING))
                            (UPPER_TRIANGLE_3 + Vector2(0.0f, Y_PADDING_FOR_GRADIENT))
                            (UPPER_TRIANGLE_3 + Vector2(0.0f, Y_PADDING_FOR_GRADIENT)))
                        (Quad.color Colors.blue)

                    let LOWER_LEFT_1 = Vector2(l + 0.1f * w, t + (0.95f - TRIANGLE_HEIGHT) * w)

                    let LOWER_LEFT_2 =
                        Vector2(l + (0.1f + LOWER_X_THICKNESS) * w, t + (0.95f - TRIANGLE_HEIGHT) * w)

                    let LOWER_LEFT_3 = Vector2(l + 0.5f * w, t + (0.95f - LOWER_Y_THICKNESS) * w)
                    let LOWER_LEFT_4 = Vector2(l + 0.5f * w, t + 0.95f * w)

                    Draw.untextured_quad
                        (Quad.create
                            (LOWER_LEFT_1 + Vector2(-X_PADDING_FOR_GRADIENT, -PADDING))
                            (LOWER_LEFT_2 + Vector2(X_PADDING_FOR_GRADIENT_INNER, -PADDING))
                            (LOWER_LEFT_3 + Vector2(0.0f, -Y_PADDING_FOR_GRADIENT))
                            (LOWER_LEFT_4 + Vector2(0.0f, Y_PADDING_FOR_GRADIENT))
                         |> Quad.translate (0.0f, breathe_2))
                        (Quad.color Colors.blue)

                    let LOWER_RIGHT_1 = Vector2(r - 0.1f * w, t + (0.95f - TRIANGLE_HEIGHT) * w)

                    let LOWER_RIGHT_2 =
                        Vector2(r - (0.1f + LOWER_X_THICKNESS) * w, t + (0.95f - TRIANGLE_HEIGHT) * w)

                    let LOWER_RIGHT_3 = Vector2(r - 0.5f * w, t + (0.95f - LOWER_Y_THICKNESS) * w)
                    let LOWER_RIGHT_4 = Vector2(r - 0.5f * w, t + 0.95f * w)

                    Draw.untextured_quad
                        (Quad.create
                            (LOWER_RIGHT_1 + Vector2(X_PADDING_FOR_GRADIENT, -PADDING))
                            (LOWER_RIGHT_2 + Vector2(-X_PADDING_FOR_GRADIENT_INNER, -PADDING))
                            (LOWER_RIGHT_3 + Vector2(0.0f, -Y_PADDING_FOR_GRADIENT))
                            (LOWER_RIGHT_4 + Vector2(0.0f, Y_PADDING_FOR_GRADIENT))
                         |> Quad.translate (0.0f, breathe_2))
                        (Quad.color Colors.blue)

                    /// STENCIL FOR LIGHT BLUE PARTS WITH RAIN AND VISUALISER IN THEM

                    Stencil.start_stencilling (true)

                    // center triangle
                    Draw.untextured_quad
                        (Quad.create UPPER_TRIANGLE_1 UPPER_TRIANGLE_2 UPPER_TRIANGLE_3 UPPER_TRIANGLE_3)
                        (Quad.color Colors.cyan_accent)

                    Draw.untextured_quad
                        (Quad.create LOWER_LEFT_1 LOWER_LEFT_2 LOWER_LEFT_3 LOWER_LEFT_4
                         |> Quad.translate (0.0f, breathe_2))
                        (Quad.color Colors.cyan_accent)

                    Draw.untextured_quad
                        (Quad.create LOWER_RIGHT_1 LOWER_RIGHT_2 LOWER_RIGHT_3 LOWER_RIGHT_4
                         |> Quad.translate (0.0f, breathe_2))
                        (Quad.color Colors.cyan_accent)

                    Draw.sprite breathe_bounds Colors.white (Content.Texture "logo")

                    /// RENDER VISUALISER AND RAIN INSIDE STENCIL

                    Stencil.start_drawing ()
                    Draw.rect breathe_bounds Colors.cyan_accent
                    let rain = Content.Texture "rain"
                    let v = float32 counter.Time
                    let q = breathe_bounds.AsQuad

                    let draw_tiling_rain (scale, x, y) color =
                        let stride = float32 rain.Width * scale
                        let mutable y = breathe_bounds.Top - stride + y % stride

                        while y < breathe_bounds.Bottom do
                            let mutable x = breathe_bounds.Left - stride + x % stride

                            while x < breathe_bounds.Right do
                                Draw.sprite (Rect.Box(x, y, stride, stride)) color rain
                                x <- x + stride

                            y <- y + stride

                    draw_tiling_rain (0.625f, v * 0.06f, v * 0.07f) Colors.blue.O2
                    draw_tiling_rain (1.0f, v * 0.1f, v * 0.11f) Colors.blue.O3
                    draw_tiling_rain (1.5625f, v * 0.15f, v * 0.16f) Colors.blue

                    let mutable prev = 0.0f
                    let m = b - w * 0.5f

                    for i in 0..31 do
                        let level =
                            (seq { (i * 8) .. (i * 8 + 7) }
                             |> Seq.map (fun x -> Devices.waveform.[x])
                             |> Seq.sum)
                            * 0.1f

                        let i = float32 i

                        Draw.untextured_quad
                            (Quad.create
                                (new Vector2(l + i * w / 32.0f, m - prev))
                                (new Vector2(l + (i + 1.0f) * w / 32.0f, m - level))
                                (new Vector2(l + (i + 1.0f) * w / 32.0f, b))
                                (new Vector2(l + i * w / 32.0f, b)))
                            (Quad.color (Colors.blue_accent.O3))

                        prev <- level

                    Stencil.finish ()
                    Draw.sprite breathe_bounds Colors.white (Content.Texture "logo")

        member this.Move(l, t, r, b) =
            this.Position <-
                {
                    Left = 0.5f %+ l
                    Top = 0.5f %+ t
                    Right = 0.5f %+ r
                    Bottom = 0.5f %+ b
                }

        override this.Update(elapsed_ms, moved) =
            base.Update(elapsed_ms, moved)

            if moved then
                match state with
                | Centre -> this.Move(-400.0f, -400.0f, 400.0f, 400.0f)
                | Hidden -> this.Move(-Viewport.vwidth * 0.5f - 600.0f, -300.0f, -Viewport.vwidth * 0.5f, 300.0f)
                | Menu -> this.Move(-Viewport.vwidth * 0.5f, -400.0f, 800.0f - Viewport.vwidth * 0.5f, 400.0f)

            counter.Update elapsed_ms

    let display =
        Display(
            Position =
                {
                    Left = 0.5f %- 300.0f
                    Top = 0.5f %+ 1000.0f
                    Right = 0.5f %+ 300.0f
                    Bottom = 0.5f %+ 1600.0f
                }
        )

    let move_center () =
        state <- Centre
        display.Move(-400.0f, -400.0f, 400.0f, 400.0f)

    let move_offscreen () =
        state <- Hidden
        display.Move(-Viewport.vwidth * 0.5f - 600.0f, -300.0f, -Viewport.vwidth * 0.5f, 300.0f)
        display.SnapPosition()

    let move_menu () =
        state <- Menu
        display.Move(-Viewport.vwidth * 0.5f, -400.0f, 800.0f - Viewport.vwidth * 0.5f, 400.0f)
