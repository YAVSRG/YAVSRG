namespace Interlude.Features.Play.HUD

open System
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skins.HudLayouts
open Interlude.Content
open Interlude.Features.Gameplay
open Interlude.Features.Play

module ProgressMeter =

    let private PIE_SEGMENTS = 30
    let private PIE_SEGMENTS_F = float32 PIE_SEGMENTS
    let private SECTOR_ANGLE = 2.0f * MathF.PI / PIE_SEGMENTS_F

    let draw_pie (bounds: Rect, color_fg: Color, color_bg: Color, progress: float32) : unit =
        let x, y = bounds.Center
        let r = (min bounds.Width bounds.Height) * 0.5f

        let outer i =
            let angle = float32 i * SECTOR_ANGLE
            let struct (a, b) = MathF.SinCos(angle)
            (x + r * a, y - r * b)

        let inner_exact (i: float32) =
            let angle = i * SECTOR_ANGLE
            let struct (a, b) = MathF.SinCos(angle)
            (x + (r - 4f) * a, y - (r - 4f) * b)

        let inner (i: int32) = inner_exact (float32 i)

        for i = 1 to PIE_SEGMENTS do
            Render.quad_points
                (x, y)
                (x, y)
                (inner (i - 1))
                (inner i)
                color_bg

            Render.quad_points
                (inner (i - 1))
                (outer (i - 1))
                (outer i)
                (inner i)
                Colors.white.O2

        let progress_rounded_down = progress * (PIE_SEGMENTS_F - 0.1f) |> floor |> int
        for i = 1 to progress_rounded_down do
            Render.quad_points
                (x, y)
                (x, y)
                (inner (i - 1))
                (inner i)
                color_fg

        Render.quad_points
            (x, y)
            (x, y)
            (inner progress_rounded_down)
            (inner_exact (progress * PIE_SEGMENTS_F))
            color_fg

    let draw_percent_progress_centered (texture: Sprite, bounds: Rect, color: Color, progress: float32, spacing: float32, percent_spacing: float32) : unit =
        let progress_text = sprintf "%.0f%%" (progress * 100.0f)
        let char_width = float32 texture.Width
        let width = (percent_spacing + float32 progress_text.Length + (float32 progress_text.Length - 1.0f) * spacing) * char_width
        let height = float32 texture.Height
        let scale = min (bounds.Width / width) (bounds.Height / height)

        let mutable char_bounds =
            Rect.FromSize(
                bounds.CenterX - width * scale * 0.5f,
                bounds.CenterY - height * scale * 0.5f,
                char_width * scale,
                height * scale
            )

        for c in progress_text do
            if c = '%' then
                char_bounds <- char_bounds.Translate(scale * percent_spacing * char_width, 0.0f)
                Render.tex_quad char_bounds.AsQuad color.AsQuad (Sprite.pick_texture (0, 12) texture)
            else
                Render.tex_quad char_bounds.AsQuad color.AsQuad (Sprite.pick_texture (0, int (c - '0')) texture)
                char_bounds <- char_bounds.Translate(scale * (1.0f + spacing) * char_width, 0.0f)

    let fmt_time_left (time_left: float32<ms / rate>) : string =
        sprintf
            "%i:%02i"
            (time_left / 60000.0f<ms / rate> |> floor |> int)
            ((time_left % 60000.0f<ms / rate>) / 1000.0f<ms / rate> |> floor |> int)

    let draw_countdown_centered (texture: Sprite, bounds: Rect, color: Color, time_left: float32<ms / rate>, spacing: float32, colon_spacing: float32) : unit =
        let time_left_text = fmt_time_left time_left

        let char_width = float32 texture.Width
        let width = (colon_spacing * 2.0f + float32 time_left_text.Length + (float32 time_left_text.Length - 1.0f) * spacing) * char_width
        let height = float32 texture.Height
        let scale = min (bounds.Width / width) (bounds.Height / height)

        let mutable char_bounds =
            Rect.FromSize(
                bounds.CenterX - width * scale * 0.5f,
                bounds.CenterY - height * scale * 0.5f,
                char_width * scale,
                height * scale
            )

        for c in time_left_text do
            if c = ':' then
                char_bounds <- char_bounds.Translate(scale * colon_spacing * char_width, 0.0f)
                Render.tex_quad char_bounds.AsQuad color.AsQuad (Sprite.pick_texture (0, 11) texture)
                char_bounds <- char_bounds.Translate(scale * (1.0f + spacing + colon_spacing) * char_width, 0.0f)
            else
                Render.tex_quad char_bounds.AsQuad color.AsQuad (Sprite.pick_texture (0, int (c - '0')) texture)
                char_bounds <- char_bounds.Translate(scale * (1.0f + spacing) * char_width, 0.0f)

type ProgressPie(config: HudConfig, state: PlayState) =
    inherit StaticWidget(NodeType.None)

    let duration =
        let chart = state.WithColors
        chart.LastNote - chart.FirstNote

    let font_texture = Content.Texture "progress-meter-font"

    override this.Draw() =
        let now = state.CurrentChartTime()
        let percent = now / duration |> max 0.0f |> min 1.0f

        ProgressMeter.draw_pie(this.Bounds.SliceT(this.Bounds.Width), config.ProgressMeterColor, config.ProgressMeterBackgroundColor, percent)

        if config.ProgressMeterUseFont then

            match config.ProgressMeterLabel with
                | ProgressPieLabel.Countdown ->
                    let time_left = (duration - now) / SelectedChart.rate.Value |> max 0.0f<ms / rate>
                    ProgressMeter.draw_countdown_centered (
                        font_texture,
                        this.Bounds.SliceB(this.Bounds.Width * config.ProgressMeterLabelSize),
                        Color.White,
                        time_left,
                        config.ProgressMeterFontSpacing,
                        config.ProgressMeterColonExtraSpacing
                    )
                | ProgressPieLabel.Percentage ->
                    ProgressMeter.draw_percent_progress_centered (
                        font_texture,
                        this.Bounds.SliceB(this.Bounds.Width * config.ProgressMeterLabelSize),
                        Color.White,
                        percent,
                        config.ProgressMeterFontSpacing,
                        config.ProgressMeterPercentExtraSpacing
                    )
                | _ -> ()

        else

            let text =
                match config.ProgressMeterLabel with
                | ProgressPieLabel.Countdown ->
                    let time_left = (duration - now) / SelectedChart.rate.Value |> max 0.0f<ms / rate>
                    ProgressMeter.fmt_time_left time_left
                | ProgressPieLabel.Percentage -> sprintf "%.0f%%" (percent * 100.0f)
                | _ -> ""

            Text.fill_b (
                Style.font,
                text,
                this.Bounds.SliceB(this.Bounds.Width * config.ProgressMeterLabelSize),
                Colors.text_subheading,
                Alignment.CENTER
            )