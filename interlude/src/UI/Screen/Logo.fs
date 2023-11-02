namespace Interlude.UI

open System
open OpenTK.Mathematics
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Audio
open Percyqaz.Flux.UI
open Interlude

module Logo =

    type private State =
        | Centre
        | Menu
        | Hidden

    let mutable private state = Centre

    type Display() =
        inherit DynamicContainer(NodeType.None)

        let counter = Animation.Counter(10000000.0)

        override this.Draw() =
            if state = Hidden then
                ()
            else
                base.Draw()
                let w = this.Bounds.Width

                let breathe_1 = float32 (Math.Sin(counter.Time / 3500.0 * Math.PI) * 0.01) * w

                let breathe_2 =
                    float32 (Math.Cos(counter.Time / 8000.0 * Math.PI) * 0.018 + 0.018) * w

                let breathe_bounds = this.Bounds.Translate(0.0f, breathe_1)

                let {
                        Rect.Left = l
                        Top = t
                        Right = r
                        Bottom = b
                    } =
                    breathe_bounds

                if r > 2.0f then

                    Draw.quad
                        (Quad.createv
                            (l + 0.08f * w, t + 0.09f * w)
                            (l + 0.5f * w, t + 0.76875f * w)
                            (l + 0.5f * w, t + 0.76875f * w)
                            (r - 0.08f * w, t + 0.09f * w))
                        (Quad.color Colors.blue)
                        Sprite.DefaultQuad

                    Draw.quad
                        (Quad.createv
                            (l + 0.08f * w, t + 0.29f * w)
                            (l + 0.22f * w, t + 0.29f * w) // todo: 0.22 is too far. go and work out the right number
                            (l + 0.5f * w, t + 0.75f * w)
                            (l + 0.5f * w, t + 0.96875f * w)
                         |> Quad.translate (0.0f, breathe_2))
                        (Quad.color Colors.blue)
                        Sprite.DefaultQuad

                    Draw.quad
                        (Quad.createv
                            (r - 0.08f * w, t + 0.29f * w)
                            (r - 0.22f * w, t + 0.29f * w)
                            (l + 0.5f * w, t + 0.75f * w)
                            (l + 0.5f * w, t + 0.96875f * w)
                         |> Quad.translate (0.0f, breathe_2))
                        (Quad.color Colors.blue)
                        Sprite.DefaultQuad

                    Stencil.start_stencilling (true)

                    Draw.quad
                        (Quad.createv
                            (l + 0.1f * w, t + 0.1f * w)
                            (l + 0.5f * w, t + 0.75f * w)
                            (l + 0.5f * w, t + 0.75f * w)
                            (r - 0.1f * w, t + 0.1f * w))
                        (Quad.color Colors.cyan_accent)
                        Sprite.DefaultQuad

                    Draw.quad
                        (Quad.createv
                            (l + 0.1f * w, t + 0.3f * w)
                            (l + 0.2075f * w, t + 0.3f * w)
                            (l + 0.5f * w, t + 0.77f * w)
                            (l + 0.5f * w, t + 0.95f * w)
                         |> Quad.translate (0.0f, breathe_2))
                        (Quad.color Colors.cyan_accent)
                        Sprite.DefaultQuad

                    Draw.quad
                        (Quad.createv
                            (r - 0.1f * w, t + 0.3f * w)
                            (r - 0.2075f * w, t + 0.3f * w)
                            (l + 0.5f * w, t + 0.77f * w)
                            (l + 0.5f * w, t + 0.95f * w)
                         |> Quad.translate (0.0f, breathe_2))
                        (Quad.color Colors.cyan_accent)
                        Sprite.DefaultQuad

                    Draw.sprite breathe_bounds Colors.white (Content.get_texture "logo")

                    Stencil.start_drawing ()
                    //chart background
                    Draw.rect breathe_bounds Colors.cyan_accent
                    let rain = Content.get_texture "rain"
                    let v = float32 counter.Time
                    let q = Quad.ofRect breathe_bounds

                    Draw.quad
                    <| q
                    <| Quad.color (Colors.blue.O2)
                    <| rain.WithUV(Sprite.tiling_uv (0.625f, v * 0.06f, v * 0.07f) rain q)

                    Draw.quad
                    <| q
                    <| Quad.color (Colors.blue.O3)
                    <| rain.WithUV(Sprite.tiling_uv (1.0f, v * 0.1f, v * 0.11f) rain q)

                    Draw.quad
                    <| q
                    <| Quad.color (Colors.blue)
                    <| rain.WithUV(Sprite.tiling_uv (1.5625f, v * 0.15f, v * 0.16f) rain q)

                    let mutable prev = 0.0f
                    let m = b - w * 0.5f

                    for i in 0..31 do
                        let level =
                            (seq { (i * 8) .. (i * 8 + 7) }
                             |> Seq.map (fun x -> Devices.waveform.[x])
                             |> Seq.sum)
                            * 0.1f

                        let i = float32 i

                        Draw.quad
                            (Quad.create
                                (new Vector2(l + i * w / 32.0f, m - prev))
                                (new Vector2(l + (i + 1.0f) * w / 32.0f, m - level))
                                (new Vector2(l + (i + 1.0f) * w / 32.0f, b))
                                (new Vector2(l + i * w / 32.0f, b)))
                            (Quad.color (Colors.blue_accent.O3))
                            Sprite.DefaultQuad

                        prev <- level

                    Stencil.finish ()
                    Draw.sprite breathe_bounds Colors.white (Content.get_texture "logo")

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
