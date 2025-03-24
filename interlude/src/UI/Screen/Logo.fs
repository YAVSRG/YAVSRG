namespace Interlude.UI

open System
open OpenTK.Mathematics
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Audio
open Percyqaz.Flux.UI
open Interlude.Content

type Logo() =
    inherit SlideContainer(NodeType.None)

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

    let OFFSCREEN_BELOW : Position = { Left = 0.5f %- 300.0f; Top = 0.5f %+ 1000.0f; Right = 0.5f %+ 300.0f; Bottom = 0.5f %+ 1600.0f }
    let CENTER : Position = { Left = 0.5f %- 400.0f; Top = 0.5f %- 400.0f; Right = 0.5f %+ 400.0f; Bottom = 0.5f %+ 400.0f }
    let OFFSCREEN_LEFT : Position = { Left = 0.0f %- 610.0f; Top = 0.5f %- 300.0f; Right = 0.0f %- 100.0f; Bottom = 0.5f %+ 300.0f }
    let MENU : Position = { Left = 0.0f %- 0.0f; Top = 0.5f %- 400.0f; Right = 0.0f %+ 800.0f; Bottom = 0.5f %+ 400.0f }

    let WAVE_HEIGHT = 20.0f
    let WAVE_SPEED = 0.002f
    let WAVE_SCALE = 0.2f

    let WAVE_HEIGHT_2 = 8.0f
    let WAVE_SPEED_2 = -0.0057f
    let WAVE_SCALE_2 = 0.73f

    let counter = Animation.Counter(10000000.0)

    override this.Init(parent: Widget) : unit =
        this.Position <- OFFSCREEN_BELOW
        base.Init parent

    override this.Draw() =
        if this.Bounds.Right < 0.0f then
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

                Render.quad_vecs
                    (UPPER_TRIANGLE_1 + Vector2(-X_PADDING_FOR_GRADIENT, -PADDING))
                    (UPPER_TRIANGLE_2 + Vector2(X_PADDING_FOR_GRADIENT, -PADDING))
                    (UPPER_TRIANGLE_3 + Vector2(0.0f, Y_PADDING_FOR_GRADIENT))
                    (UPPER_TRIANGLE_3 + Vector2(0.0f, Y_PADDING_FOR_GRADIENT))
                    Colors.blue

                let LOWER_LEFT_1 = Vector2(l + 0.1f * w, t + (0.95f - TRIANGLE_HEIGHT) * w)

                let LOWER_LEFT_2 =
                    Vector2(l + (0.1f + LOWER_X_THICKNESS) * w, t + (0.95f - TRIANGLE_HEIGHT) * w)

                let LOWER_LEFT_3 = Vector2(l + 0.5f * w, t + (0.95f - LOWER_Y_THICKNESS) * w)
                let LOWER_LEFT_4 = Vector2(l + 0.5f * w, t + 0.95f * w)

                Render.quad_vecs
                    (LOWER_LEFT_1 + Vector2(-X_PADDING_FOR_GRADIENT, -PADDING + breathe_2))
                    (LOWER_LEFT_2 + Vector2(X_PADDING_FOR_GRADIENT_INNER, -PADDING + breathe_2))
                    (LOWER_LEFT_3 + Vector2(0.0f, -Y_PADDING_FOR_GRADIENT + breathe_2))
                    (LOWER_LEFT_4 + Vector2(0.0f, Y_PADDING_FOR_GRADIENT + breathe_2))
                    Colors.blue

                let LOWER_RIGHT_1 = Vector2(r - 0.1f * w, t + (0.95f - TRIANGLE_HEIGHT) * w)

                let LOWER_RIGHT_2 =
                    Vector2(r - (0.1f + LOWER_X_THICKNESS) * w, t + (0.95f - TRIANGLE_HEIGHT) * w)

                let LOWER_RIGHT_3 = Vector2(r - 0.5f * w, t + (0.95f - LOWER_Y_THICKNESS) * w)
                let LOWER_RIGHT_4 = Vector2(r - 0.5f * w, t + 0.95f * w)

                Render.quad_vecs
                    (LOWER_RIGHT_1 + Vector2(X_PADDING_FOR_GRADIENT, -PADDING + breathe_2))
                    (LOWER_RIGHT_2 + Vector2(-X_PADDING_FOR_GRADIENT_INNER, -PADDING + breathe_2))
                    (LOWER_RIGHT_3 + Vector2(0.0f, -Y_PADDING_FOR_GRADIENT + breathe_2))
                    (LOWER_RIGHT_4 + Vector2(0.0f, Y_PADDING_FOR_GRADIENT + breathe_2))
                    Colors.blue

                // STENCIL FOR LIGHT BLUE PARTS WITH RAIN AND VISUALISER IN THEM

                Render.stencil_create(true)

                // center triangle
                Render.quad_vecs
                    UPPER_TRIANGLE_1
                    UPPER_TRIANGLE_2
                    UPPER_TRIANGLE_3
                    UPPER_TRIANGLE_3
                    Colors.cyan_accent

                Render.quad
                    (Quad.from_vectors(LOWER_LEFT_1, LOWER_LEFT_2, LOWER_LEFT_3, LOWER_LEFT_4)
                        |> Quad.translate (0.0f, breathe_2))
                    Colors.cyan_accent

                Render.quad
                    (Quad.from_vectors(LOWER_RIGHT_1, LOWER_RIGHT_2, LOWER_RIGHT_3, LOWER_RIGHT_4)
                        |> Quad.translate (0.0f, breathe_2))
                    Colors.cyan_accent

                Render.sprite breathe_bounds Colors.white (Content.Texture "logo")

                // RENDER VISUALISER AND RAIN INSIDE STENCIL

                Render.stencil_begin_draw()
                Render.rect breathe_bounds Colors.cyan_accent
                let rain = Content.Texture "rain"
                let RAIN_SCALE = 512.0f
                let v = float32 counter.Time

                let draw_tiling_rain (scale, x, y) color =
                    let stride = RAIN_SCALE * scale
                    let mutable y = breathe_bounds.Top - stride + y % stride

                    while y < breathe_bounds.Bottom do
                        let mutable x = breathe_bounds.Left - stride + x % stride

                        while x < breathe_bounds.Right do
                            Render.sprite (Rect.FromSize(x, y, stride, stride)) color rain
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
                            |> Seq.map (fun x -> Audio.waveform.[x])
                            |> Seq.sum)
                        * 0.125f
                        + MathF.Sin(v * WAVE_SPEED + float32 i * WAVE_SCALE) * WAVE_HEIGHT
                        + MathF.Sin(v * WAVE_SPEED_2 + float32 i * WAVE_SCALE_2) * WAVE_HEIGHT_2

                    let i = float32 i

                    Render.quad_points
                        (l + i * w / 32.0f, m - prev)
                        (l + (i + 1.0f) * w / 32.0f, m - level)
                        (l + (i + 1.0f) * w / 32.0f, b)
                        (l + i * w / 32.0f, b)
                        Colors.blue_accent.O3

                    prev <- level

                Render.stencil_finish ()
                Render.sprite breathe_bounds Colors.white (Content.Texture "logo")

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        counter.Update elapsed_ms

    member this.MoveCenter() : unit =
        this.Position <- CENTER

    member this.MoveOffscreen() : unit =
        this.Position <- OFFSCREEN_LEFT
        this.SnapPosition()

    member this.MoveMenu() : unit =
        this.Position <- MENU