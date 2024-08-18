namespace Interlude.Features.Play

open Percyqaz.Flux.UI
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Graphics
open Prelude.Gameplay.Mods
open Interlude.UI
open Interlude.Features.Gameplay

type SongInfo(info: LoadedChartInfo, on_ready: unit -> unit) =
    inherit StaticWidget(NodeType.None)

    let mod_string = Mods.format (SelectedChart.rate.Value, info.WithColors.ModsApplied, false)

    let BG_SCALE = 0.35f

    let timer = Animation.Delay (float Song.LEADIN_TIME * 0.5)
    let fade = Animation.Fade 1.0f

    //let subrange lo hi x = (x - lo) / (hi - lo) |> min 1.0f |> max 0.0f
    //let decelerate x = (2.0f - x) * x
    //let accelerate x = x * x
    //let range lo hi x = lo + x * (hi - lo)

    override this.Draw() =
        let alpha = fade.Alpha

        let bg_panel = this.Bounds.SlicePercentX(BG_SCALE).SlicePercentY(BG_SCALE).Translate(0.0f, -120.0f)
        let song_title = bg_panel.BorderB(80.0f).ExpandX(200.0f)

        match Background.get_current() with
        | Some bg ->
            let scale = max (bg_panel.Height / float32 bg.Height) (bg_panel.Width / float32 bg.Width)
            let x_adjust = (float32 bg.Width * scale - bg_panel.Width) * 0.5f
            Draw.quad bg_panel.AsQuad (Colors.white.O4a(alpha).AsQuad) (Sprite.tiling (scale, bg_panel.Left - x_adjust, bg_panel.Top) bg bg_panel.AsQuad)
            Draw.rect bg_panel (Colors.shadow_2.O1a alpha)
        | None -> ()

        let text_color = (Colors.white.O4a alpha, Colors.shadow_2.O4a alpha)
        let text_subheading_color = (Colors.grey_1.O4a alpha, Colors.shadow_2.O4a alpha)

        Text.fill_b(Style.font, info.CacheInfo.Title, song_title, text_color, Alignment.CENTER)
        Text.fill_b(Style.font, info.CacheInfo.Artist + "  •  " + info.CacheInfo.Creator, song_title.TranslateY(70.0f).SliceT(45.0f), text_subheading_color, Alignment.CENTER)
        Text.fill_b(Style.font, mod_string, song_title.TranslateY(70.0f).SliceB(45.0f), text_subheading_color, Alignment.CENTER)

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        timer.Update elapsed_ms

        if fade.Target <> 0.0f && timer.Complete then
            on_ready()
            fade.Target <- 0.0f

        fade.Update elapsed_ms