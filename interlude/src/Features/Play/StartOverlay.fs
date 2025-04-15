namespace Interlude.Features.Play

open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Mods
open Interlude.UI
open Interlude.Features.Gameplay
open Interlude.Features.Pacemaker

type StartOverlay(info: LoadedChartInfo, pacemaker: PacemakerState, on_ready: unit -> unit) =
    inherit StaticWidget(NodeType.None)

    let mod_string = ModState.format (SelectedChart.rate.Value, info.WithColors.ModsApplied)

    let BG_SCALE = 0.35f

    let timer = Animation.Delay (float Song.LEADIN_TIME * 0.5)
    let fade = Animation.Fade 1.0f

    let pacemaker_desc = PacemakerState.description pacemaker

    override this.Draw() =
        let alpha = fade.Alpha

        if info.WithMods.Status <> ModStatus.Ranked then
            let status, desc, color =
                match info.WithMods.Status with
                | ModStatus.Unstored -> %"mods.mod_status.unstored", %"mods.mod_status.unstored.desc", Colors.red_accent
                | ModStatus.Unranked -> %"mods.mod_status.unranked", %"mods.mod_status.unranked.desc", Colors.yellow_accent
                | ModStatus.Offline -> %"mods.mod_status.offline", %"mods.mod_status.offline.desc", Colors.yellow_accent
                | ModStatus.Ranked -> failwith "impossible"

            let banner_area = this.Bounds.SliceT(100.0f, 100.0f)

            Render.rect_c (this.Bounds.SliceT(600.0f)) (Quad.gradient_top_to_bottom (color.O1a alpha) color.O0)
            Render.rect (banner_area.SlicePercentY(fade.Value)) (Colors.shadow_2.O2a alpha)
            Text.fill_b(Style.font, status, banner_area.Shrink(100.0f, Style.PADDING * 2.0f).SlicePercentT(0.7f), (color.O4a alpha, Colors.shadow_2.O4a alpha), Alignment.CENTER)
            Text.fill_b(Style.font, desc, banner_area.Shrink(100.0f, Style.PADDING * 2.0f).ShrinkPercentT(0.6f), (color.O4a alpha, Colors.shadow_2.O4a alpha), Alignment.CENTER)

        let bg_panel = this.Bounds.SlicePercentX(BG_SCALE).SlicePercentY(BG_SCALE).Translate(0.0f, -120.0f)
        let song_title = bg_panel.BorderB(80.0f).ExpandX(200.0f)

        match Background.get_current() with
        | Some bg ->
            let scale = max (bg_panel.Height / float32 bg.Height) (bg_panel.Width / float32 bg.Width)
            let x_adjust = (float32 bg.Width * scale - bg_panel.Width) * 0.5f
            let y_adjust = (float32 bg.Height * scale - bg_panel.Height) * 0.5f
            Render.tex_quad bg_panel.AsQuad (Colors.white.O4a(alpha).AsQuad) (Sprite.tiling (scale, bg_panel.Left - x_adjust, bg_panel.Top - y_adjust) bg bg_panel.AsQuad)
            Render.rect bg_panel (Colors.shadow_2.O1a alpha)
        | None -> ()

        let text_color = (Colors.white.O4a alpha, Colors.shadow_2.O4a alpha)
        let text_subheading_color = (Colors.grey_1.O4a alpha, Colors.shadow_2.O4a alpha)

        Text.fill_b(Style.font, info.ChartMeta.Title, song_title, text_color, Alignment.CENTER)
        Text.fill_b(Style.font, info.ChartMeta.Artist + "  •  " + info.ChartMeta.Creator, song_title.TranslateY(70.0f).SliceT(40.0f), text_subheading_color, Alignment.CENTER)
        Text.fill_b(Style.font, mod_string, song_title.TranslateY(70.0f).SliceB(40.0f), text_subheading_color, Alignment.CENTER)
        Text.fill_b(Style.font, pacemaker_desc, song_title.TranslateY(170.0f).SliceT(60.0f), (Colors.yellow_accent.O4a alpha, Colors.shadow_2.O4a alpha), Alignment.CENTER)

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        timer.Update elapsed_ms

        if fade.Target <> 0.0f && (timer.Complete || (%%"skip").Held()) then
            on_ready()
            fade.Target <- 0.0f

        fade.Update elapsed_ms