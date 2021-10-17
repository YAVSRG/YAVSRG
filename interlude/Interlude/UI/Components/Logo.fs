namespace Interlude.UI.Components

open System.Drawing
open OpenTK.Mathematics
open Interlude.Graphics
open Interlude
open Interlude.UI
open Interlude.UI.Animation

module Logo =
    
    type Display() as this =
        inherit Widget()

        let counter = AnimationCounter(10000000.0)
        do this.Animation.Add(counter)

        override this.Draw() =
            base.Draw()
            let w = Rect.width this.Bounds
            let struct (l, t, r, b) = this.Bounds

            if (r > 0.0f) then

                Draw.quad
                    (Quad.create(new Vector2(l + 0.08f * w, t + 0.09f * w)) (new Vector2(l + 0.5f * w, t + 0.76875f * w)) (new Vector2(l + 0.5f * w, t + 0.76875f * w)) (new Vector2(r - 0.08f * w, t + 0.09f * w)))
                    (Quad.colorOf(Color.DarkBlue))
                    Sprite.DefaultQuad
                Draw.quad
                    (Quad.create(new Vector2(l + 0.08f * w, t + 0.29f * w)) (new Vector2(l + 0.22f * w, t + 0.29f * w)) (new Vector2(l + 0.5f * w, t + 0.76875f * w)) (new Vector2(l + 0.5f * w, t + 0.96875f * w)))
                    (Quad.colorOf(Color.DarkBlue))
                    Sprite.DefaultQuad
                Draw.quad
                    (Quad.create(new Vector2(r - 0.08f * w, t + 0.29f * w)) (new Vector2(r - 0.22f * w, t + 0.29f * w)) (new Vector2(l + 0.5f * w, t + 0.76875f * w)) (new Vector2(l + 0.5f * w, t + 0.96875f * w)))
                    (Quad.colorOf(Color.DarkBlue))
                    Sprite.DefaultQuad

                Stencil.create(true)
                Draw.quad
                    (Quad.create(new Vector2(l + 0.1f * w, t + 0.1f * w)) (new Vector2(l + 0.5f * w, t + 0.75f * w)) (new Vector2(l + 0.5f * w, t + 0.75f * w)) (new Vector2(r - 0.1f * w, t + 0.1f * w)))
                    (Quad.colorOf(Color.Aqua))
                    Sprite.DefaultQuad
                Draw.quad
                    (Quad.create(new Vector2(l + 0.1f * w, t + 0.3f * w)) (new Vector2(l + 0.2f * w, t + 0.3f * w)) (new Vector2(l + 0.5f * w, t + 0.7875f * w)) (new Vector2(l + 0.5f * w, t + 0.95f * w)))
                    (Quad.colorOf(Color.Aqua))
                    Sprite.DefaultQuad
                Draw.quad
                    (Quad.create(new Vector2(r - 0.1f * w, t + 0.3f * w)) (new Vector2(r - 0.2f * w, t + 0.3f * w)) (new Vector2(l + 0.5f * w, t + 0.7875f * w)) (new Vector2(l + 0.5f * w, t + 0.95f * w)))
                    (Quad.colorOf(Color.Aqua))
                    Sprite.DefaultQuad
                Draw.rect this.Bounds Color.White (Content.getTexture "logo")

                Stencil.draw()
                //chart background
                Draw.rect this.Bounds Color.Aqua Sprite.Default
                let rain = Content.getTexture "rain"
                let v = float32 counter.Time
                let q = Quad.ofRect this.Bounds
                Draw.quad <| q <| Quad.colorOf (Color.FromArgb(80, 0, 0, 255))  <| rain.WithUV(Sprite.tilingUV(0.625f, v * 0.06f, v * 0.07f) rain q)
                Draw.quad <| q <| Quad.colorOf (Color.FromArgb(150, 0, 0, 255)) <| rain.WithUV(Sprite.tilingUV(1.0f, v * 0.1f, v * 0.11f) rain q)
                Draw.quad <| q <| Quad.colorOf (Color.FromArgb(220, 0, 0, 255)) <| rain.WithUV(Sprite.tilingUV(1.5625f, v * 0.15f, v * 0.16f) rain q)

                let mutable prev = 0.0f
                let m = b - w * 0.5f
                for i in 0 .. 31 do
                    let level =
                        (seq { (i * 8) .. (i * 8 + 7) }
                        |> Seq.map (fun x -> Audio.waveForm.[x])
                        |> Seq.sum) * 0.1f
                    let i = float32 i
                    Draw.quad
                        (Quad.create(new Vector2(l + i * w / 32.0f, m - prev)) (new Vector2(l + (i + 1.0f) * w / 32.0f, m - level)) (new Vector2(l + (i + 1.0f) * w / 32.0f, b)) (new Vector2(l + i * w / 32.0f, b)))
                        (Quad.colorOf(Color.FromArgb(127, 0, 0, 255)))
                        Sprite.DefaultQuad
                    prev <- level

                Stencil.finish()
                Draw.rect this.Bounds Color.White (Content.getTexture "logo")

    let display = Display() |> positionWidget(-300.0f, 0.5f, 1000.0f, 0.5f, 300.0f, 0.5f, 1600.0f, 0.5f)

    let moveCentre () = display.Move (-400.0f, -400.0f, 400.0f, 400.0f)

    let moveOffscreen () = display.Move (-Render.vwidth * 0.5f - 600.0f, -300.0f, -Render.vwidth * 0.5f, 300.0f)

    let moveMenu () = display.Move (-Render.vwidth * 0.5f, -400.0f, 800.0f - Render.vwidth * 0.5f, 400.0f)