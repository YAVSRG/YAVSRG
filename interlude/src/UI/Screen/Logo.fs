namespace Interlude.UI

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
            base.Draw()
            let w = this.Bounds.Width
            let { Rect.Left = l; Top = t; Right = r; Bottom = b } = this.Bounds

            if r > 2.0f then

                Draw.quad
                    (Quad.create(new Vector2(l + 0.08f * w, t + 0.09f * w)) (new Vector2(l + 0.5f * w, t + 0.76875f * w)) (new Vector2(l + 0.5f * w, t + 0.76875f * w)) (new Vector2(r - 0.08f * w, t + 0.09f * w)))
                    (Quad.colorOf(Colors.blue))
                    Sprite.DefaultQuad
                Draw.quad
                    (Quad.create(new Vector2(l + 0.08f * w, t + 0.29f * w)) (new Vector2(l + 0.22f * w, t + 0.29f * w)) (new Vector2(l + 0.5f * w, t + 0.76875f * w)) (new Vector2(l + 0.5f * w, t + 0.96875f * w)))
                    (Quad.colorOf(Colors.blue))
                    Sprite.DefaultQuad
                Draw.quad
                    (Quad.create(new Vector2(r - 0.08f * w, t + 0.29f * w)) (new Vector2(r - 0.22f * w, t + 0.29f * w)) (new Vector2(l + 0.5f * w, t + 0.76875f * w)) (new Vector2(l + 0.5f * w, t + 0.96875f * w)))
                    (Quad.colorOf(Colors.blue))
                    Sprite.DefaultQuad

                Stencil.create(true)
                Draw.quad
                    (Quad.create(new Vector2(l + 0.1f * w, t + 0.1f * w)) (new Vector2(l + 0.5f * w, t + 0.75f * w)) (new Vector2(l + 0.5f * w, t + 0.75f * w)) (new Vector2(r - 0.1f * w, t + 0.1f * w)))
                    (Quad.colorOf(Colors.cyan_accent))
                    Sprite.DefaultQuad
                Draw.quad
                    (Quad.create(new Vector2(l + 0.1f * w, t + 0.3f * w)) (new Vector2(l + 0.2075f * w, t + 0.3f * w)) (new Vector2(l + 0.5f * w, t + 0.77f * w)) (new Vector2(l + 0.5f * w, t + 0.95f * w)))
                    (Quad.colorOf(Colors.cyan_accent))
                    Sprite.DefaultQuad
                Draw.quad
                    (Quad.create(new Vector2(r - 0.1f * w, t + 0.3f * w)) (new Vector2(r - 0.2075f * w, t + 0.3f * w)) (new Vector2(l + 0.5f * w, t + 0.77f * w)) (new Vector2(l + 0.5f * w, t + 0.95f * w)))
                    (Quad.colorOf(Colors.cyan_accent))
                    Sprite.DefaultQuad
                Draw.sprite this.Bounds Colors.white (Content.getTexture "logo")

                Stencil.draw()
                //chart background
                Draw.rect this.Bounds Colors.cyan_accent
                let rain = Content.getTexture "rain"
                let v = float32 counter.Time
                let q = Quad.ofRect this.Bounds
                Draw.quad <| q <| Quad.colorOf (Colors.blue.O2)  <| rain.WithUV(Sprite.tilingUV(0.625f, v * 0.06f, v * 0.07f) rain q)
                Draw.quad <| q <| Quad.colorOf (Colors.blue.O3) <| rain.WithUV(Sprite.tilingUV(1.0f, v * 0.1f, v * 0.11f) rain q)
                Draw.quad <| q <| Quad.colorOf (Colors.blue) <| rain.WithUV(Sprite.tilingUV(1.5625f, v * 0.15f, v * 0.16f) rain q)

                let mutable prev = 0.0f
                let m = b - w * 0.5f
                for i in 0 .. 31 do
                    let level =
                        (seq { (i * 8) .. (i * 8 + 7) }
                        |> Seq.map (fun x -> Devices.waveForm.[x])
                        |> Seq.sum) * 0.1f
                    let i = float32 i
                    Draw.quad
                        (Quad.create(new Vector2(l + i * w / 32.0f, m - prev)) (new Vector2(l + (i + 1.0f) * w / 32.0f, m - level)) (new Vector2(l + (i + 1.0f) * w / 32.0f, b)) (new Vector2(l + i * w / 32.0f, b)))
                        (Quad.colorOf(Colors.blue_accent.O3))
                        Sprite.DefaultQuad
                    prev <- level

                Stencil.finish()
                Draw.sprite this.Bounds Colors.white (Content.getTexture "logo")

        member this.Move (l, t, r, b) = this.Position <- { Left = 0.5f %+ l; Top = 0.5f %+ t; Right = 0.5f %+ r; Bottom = 0.5f %+ b }

        override this.Update(elapsedTime, moved) =
            base.Update(elapsedTime, moved)
            if moved then
                match state with
                | Centre -> this.Move (-400.0f, -400.0f, 400.0f, 400.0f)
                | Hidden -> this.Move (-Viewport.vwidth * 0.5f - 600.0f, -300.0f, -Viewport.vwidth * 0.5f, 300.0f)
                | Menu -> this.Move (-Viewport.vwidth * 0.5f, -400.0f, 800.0f - Viewport.vwidth * 0.5f, 400.0f)
            counter.Update elapsedTime

    let display = Display(Position = { Left = 0.5f %- 300.0f; Top = 0.5f %+ 1000.0f; Right = 0.5f %+ 300.0f; Bottom = 0.5f %+ 1600.0f })

    let moveCentre () = state <- Centre; display.Move (-400.0f, -400.0f, 400.0f, 400.0f)
    let moveOffscreen () = state <- Hidden; display.Move (-Viewport.vwidth * 0.5f - 600.0f, -300.0f, -Viewport.vwidth * 0.5f, 300.0f)
    let moveMenu () = state <- Menu; display.Move (-Viewport.vwidth * 0.5f, -400.0f, 800.0f - Viewport.vwidth * 0.5f, 400.0f)