namespace Interlude.Features.Play

open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude.Gameplay.Mods
open Interlude.UI
open Interlude.Features.Gameplay

type SongInfo(state: PlayState) =
    inherit StaticWidget(NodeType.None)

    let mod_string = Mods.format (SelectedChart.rate.Value, state.WithColors.ModsApplied, false)

    let BG_HEIGHT = 270.0f
    let BG_WIDTH = 480.0f

    let timer = Animation.Delay 2000.0

    let subrange lo hi x = (x - lo) / (hi - lo) |> min 1.0f |> max 0.0f
    let decelerate x = (2.0f - x) * x
    let accelerate x = x * x
    let range lo hi x = lo + x * (hi - lo)

    override this.Draw() =

        let x, y = this.Bounds.Center

        let percent = timer.Time / timer.Interval |> float32
        let motion_x = 
            (percent |> subrange 0.0f 0.15f |> decelerate |> range -400.0f 0.0f)
            + (percent |> subrange 0.15f 0.85f |> range -30.0f 30.0f)
            + (percent |> subrange 0.85f 1.0f |> accelerate |> range 0.0f 400.0f)
        let alpha =
            (percent |> subrange 0.0f 0.2f |> decelerate |> range 0.0f 1.0f)
            + (percent |> subrange 0.8f 1.0f |> accelerate |> range 0.0f -1.0f)
            |> range 0.0f 255.0f
            |> int
            |> max 0
            |> min 255

        let bg_panel = Rect.Box(x - BG_WIDTH * 0.5f + motion_x, y - 350.0f, BG_WIDTH, BG_HEIGHT)
        let info_panel = bg_panel.BorderBottom(100.0f).Translate(0.0f, 10.0f)

        match Background.get_current() with
        | Some bg ->
            let scale = max (BG_HEIGHT / float32 bg.Height) (BG_WIDTH / float32 bg.Width)
            let x_adjust = (float32 bg.Width * scale - BG_WIDTH) * 0.5f
            Draw.quad bg_panel.AsQuad (Colors.grey_1.O4a(alpha).AsQuad) (Sprite.tiling (scale, bg_panel.Left - x_adjust, bg_panel.Top) bg bg_panel.AsQuad)
        | None -> ()

        Draw.rect info_panel (Colors.shadow_2.O4a alpha)

        let text_color = (Colors.white.O4a alpha, Colors.shadow_2.O4a alpha)
        let text_subheading_color = (Colors.grey_1.O4a alpha, Colors.shadow_2.O4a alpha)

        Text.fill_b(Style.font, state.Chart.Header.Title, info_panel.SliceTop(40.0f), text_color, Alignment.CENTER)
        Text.fill_b(Style.font, state.Chart.Header.Artist, info_panel.TrimTop(35.0f).SliceTop(40.0f), text_subheading_color, Alignment.CENTER)
        Text.fill_b(Style.font, mod_string, info_panel.SliceBottom(30.0f), text_subheading_color, Alignment.CENTER)

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        timer.Update elapsed_ms