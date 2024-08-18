namespace Interlude.Features.Play

open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Interlude.UI

type FailOverlay() =
    inherit StaticWidget(NodeType.None)

    let fade = Animation.Fade(0.0f, Target = 1.0f)

    override this.Draw() =
        let alpha = fade.Alpha

        let icon = this.Bounds.SliceY(160.0f).TranslateY(-160.0f)
        let text_color = (Colors.white.O4a alpha, Colors.shadow_2.O4a alpha)

        Draw.untextured_quad this.Bounds.AsQuad (Quad.gradient_top_to_bottom (Colors.red.O4a 0) (Colors.red.O2a alpha))

        Text.fill_b(Style.font, Icons.X_SQUARE, icon, text_color, Alignment.CENTER)
        Text.fill_b(Style.font, "Mission failed :(", icon.BorderB 120.0f, text_color, Alignment.CENTER)

        // retry
        // view score
        // next song

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        fade.Update elapsed_ms