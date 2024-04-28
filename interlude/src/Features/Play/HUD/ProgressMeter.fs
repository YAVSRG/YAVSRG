namespace Interlude.Features.Play.HUD

open System
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skinning.Noteskins
open Interlude.Content
open Interlude.Features
open Interlude.Features.Play

module ProgressMeter =

    let draw_pie (bounds: Rect, color_fg: Color, color_bg: Color, progress: float32) =
        let x, y = bounds.Center
        let r = (min bounds.Width bounds.Height) * 0.5f
        let angle = MathF.PI / 15.0f

        let outer i =
            let angle = float32 i * angle
            let struct (a, b) = MathF.SinCos(angle)
            (x + r * a, y - r * b)

        let inner i =
            let angle = float32 i * angle
            let struct (a, b) = MathF.SinCos(angle)
            (x + (r - 4f) * a, y - (r - 4f) * b)

        for i = 0 to 29 do
            Draw.untextured_quad
                (Quad.createv (x, y) (x, y) (inner i) (inner (i + 1)))
                color_bg.AsQuad

            Draw.untextured_quad
                (Quad.createv (inner i) (outer i) (outer (i + 1)) (inner (i + 1)))
                Colors.white.O2.AsQuad

        for i = 0 to progress * 29.9f |> floor |> int do
            Draw.untextured_quad
                (Quad.createv (x, y) (x, y) (inner i) (inner (i + 1)))
                color_fg.AsQuad

    let draw_percent_progress_centered(texture: Sprite, bounds: Rect, color: Color, progress: float32, spacing: float32, percent_spacing: float32) =
        let progress_text = sprintf "%.0f%%" (progress * 100.0f)
        let char_width = float32 texture.Width
        let width = (percent_spacing + float32 progress_text.Length + (float32 progress_text.Length - 1.0f) * spacing) * char_width
        let height = float32 texture.Height
        let scale = min (bounds.Width / width) (bounds.Height / height)

        let mutable char_bounds = 
            Rect.Box(
                bounds.CenterX - width * scale * 0.5f,
                bounds.CenterY - height * scale * 0.5f,
                char_width * scale,
                height * scale
            )

        for c in progress_text do
            if c = '%' then
                char_bounds <- char_bounds.Translate(scale * percent_spacing * char_width, 0.0f)
                Draw.quad char_bounds.AsQuad color.AsQuad (Sprite.pick_texture (0, 12) texture)
            else
                Draw.quad char_bounds.AsQuad color.AsQuad (Sprite.pick_texture (0, int (c - '0')) texture)
                char_bounds <- char_bounds.Translate(scale * (1.0f + spacing) * char_width, 0.0f)

    let fmt_time_left (time_left: float32<ms>) =
        sprintf
            "%i:%02i"
            (time_left / 60000.0f<ms> |> floor |> int)
            ((time_left % 60000.0f<ms>) / 1000.0f<ms> |> floor |> int)

    let draw_countdown_centered(texture: Sprite, bounds: Rect, color: Color, time_left: float32<ms>, spacing: float32, colon_spacing: float32) =
        let time_left_text = fmt_time_left time_left

        let char_width = float32 texture.Width
        let width = (colon_spacing * 2.0f + float32 time_left_text.Length + (float32 time_left_text.Length - 1.0f) * spacing) * char_width
        let height = float32 texture.Height
        let scale = min (bounds.Width / width) (bounds.Height / height)

        let mutable char_bounds = 
            Rect.Box(
                bounds.CenterX - width * scale * 0.5f,
                bounds.CenterY - height * scale * 0.5f,
                char_width * scale,
                height * scale
            )

        for c in time_left_text do
            if c = ':' then
                char_bounds <- char_bounds.Translate(scale * colon_spacing * char_width, 0.0f)
                Draw.quad char_bounds.AsQuad color.AsQuad (Sprite.pick_texture (0, 11) texture)
                char_bounds <- char_bounds.Translate(scale * (1.0f + spacing + colon_spacing) * char_width, 0.0f)
            else
                Draw.quad char_bounds.AsQuad color.AsQuad (Sprite.pick_texture (0, int (c - '0')) texture)
                char_bounds <- char_bounds.Translate(scale * (1.0f + spacing) * char_width, 0.0f)

type ProgressMeter(user_options: HUDUserOptions, noteskin_options: HUDNoteskinOptions, state: PlayState) =
    inherit StaticWidget(NodeType.None)

    let duration =
        let chart = state.WithColors
        chart.LastNote - chart.FirstNote

    let font_texture = Content.Texture "progress-meter-font"

    override this.Draw() =
        let now = state.CurrentChartTime()
        let percent = now / duration |> max 0.0f |> min 1.0f

        ProgressMeter.draw_pie(this.Bounds.SliceTop(this.Bounds.Width), noteskin_options.ProgressMeterColor, noteskin_options.ProgressMeterBackgroundColor, percent)

        if noteskin_options.ProgressMeterUseFont then

            match user_options.ProgressMeterLabel with
                | ProgressMeterLabel.Countdown ->
                    let time_left = (duration - now) / Gameplay.rate.Value |> max 0.0f<ms>
                    ProgressMeter.draw_countdown_centered (
                        font_texture,
                        this.Bounds.SliceBottom(this.Bounds.Width * noteskin_options.ProgressMeterLabelSize), 
                        Color.White,
                        time_left,
                        noteskin_options.ProgressMeterFontSpacing,
                        noteskin_options.ProgressMeterColonExtraSpacing
                    )
                | ProgressMeterLabel.Percentage ->
                    ProgressMeter.draw_percent_progress_centered (
                        font_texture,
                        this.Bounds.SliceBottom(this.Bounds.Width * noteskin_options.ProgressMeterLabelSize), 
                        Color.White,
                        percent,
                        noteskin_options.ProgressMeterFontSpacing,
                        noteskin_options.ProgressMeterPercentExtraSpacing
                    )
                | _ -> ()

        else

            let text =
                match user_options.ProgressMeterLabel with
                | ProgressMeterLabel.Countdown ->
                    let time_left = (duration - now) / Gameplay.rate.Value |> max 0.0f<ms>
                    ProgressMeter.fmt_time_left time_left
                | ProgressMeterLabel.Percentage -> sprintf "%.0f%%" (percent * 100.0f)
                | _ -> ""

            Text.fill_b (
                Style.font,
                text,
                this.Bounds.SliceBottom(this.Bounds.Width * noteskin_options.ProgressMeterLabelSize),
                Colors.text_subheading,
                Alignment.CENTER
            )