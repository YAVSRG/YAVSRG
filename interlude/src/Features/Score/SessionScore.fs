namespace Interlude.Features.Score

open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Interlude.UI

type SessionScoreBar() =
    inherit StaticWidget(NodeType.None)

    override this.Draw() =
        // box
        Draw.rect (this.Bounds.Translate(10.0f, 10.0f)) Colors.black
        Background.draw (this.Bounds, (Color.FromArgb(40, 40, 40)), 2.0f)
        Draw.rect this.Bounds (!*Palette.MAIN_100)

        // bar
        let bar = this.Bounds.SliceY(20.0f).ShrinkX(10.0f)
        Draw.rect bar Colors.shadow_2.O2
        Draw.rect (bar.SlicePercentL(0.6f)) Colors.yellow_accent

        let counter = this.Bounds.BorderB(60.0f).SliceR(400.0f)
        let counterq = 
            let struct (a, b, c, d) = counter.AsQuad
            struct (a - OpenTK.Mathematics.Vector2(30.0f, 0.0f), b, c, d)
        Draw.untextured_quad (Quad.translate (10.0f, 10.0f) counterq) Colors.black.AsQuad
        Background.drawq (counterq, (Color.FromArgb(40, 40, 40)), 2.0f)
        Draw.untextured_quad counterq (!*Palette.MAIN_100).AsQuad

        Text.fill_b(Style.font, "0062727", counter, Colors.text, Alignment.CENTER)