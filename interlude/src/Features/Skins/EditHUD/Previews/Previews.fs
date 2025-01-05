namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude
open Prelude.Skins.HudLayouts
open Interlude.Content
open Interlude.UI
open Interlude.Features.Play.HUD
open Interlude.Features.Gameplay

module SelectPreviews =

    type SelectPreview =
        (float32 * float32) option * // Optional bounds override, otherwise use user's aspect ratio
        (Rect -> unit) // Draw function

    let private create_accuracy (config: HudConfig) : SelectPreview =
        let texture = Content.Texture "accuracy-font"

        None,
        fun (bounds: Rect) ->

        if config.AccuracyUseFont then
            Accuracy.draw_accuracy_centered(
                texture,
                bounds.ShrinkB(bounds.Height * 0.4f),
                Color.White,
                Rulesets.current.FormatAccuracy 0.967234,
                config.AccuracyFontSpacing,
                config.AccuracyDotExtraSpacing,
                config.AccuracyPercentExtraSpacing
            )
        else
            Text.fill (Style.font, Rulesets.current.FormatAccuracy 0.967234, bounds.ShrinkB(bounds.Height * 0.3f), Color.White, 0.5f)

        if config.AccuracyShowName then
            Text.fill (Style.font, Rulesets.current.Name, bounds.SliceB(bounds.Height * 0.4f), Color.White, 0.5f)

    let private create_error_bar (config: HudConfig) : SelectPreview =

        Some (200.0f, 20.0f),
        fun (bounds: Rect) ->

        Render.rect (bounds.SliceL 50.0f) Color.Yellow.O1
        Render.rect (bounds.SliceR 50.0f) Color.Yellow.O1
        Render.rect (bounds.SliceX 100.0f) Color.Cyan.O1
        Render.rect (bounds.SliceX(config.TimingDisplayThickness * 2.0f * config.TimingDisplayGuideThickness)) Color.White
        Render.rect (bounds.SliceX(config.TimingDisplayThickness * 2.0f).TranslateX(30.0f)) Color.Cyan
        Render.rect (bounds.SliceX(config.TimingDisplayThickness * 2.0f).TranslateX(-80.0f).ExpandY(config.TimingDisplayReleasesExtraHeight)) Color.Yellow
        let center = bounds.CenterX
        let moving_average =
            Quad.createv
                (center - 24f, bounds.Top - 10.0f)
                (center - 24f - 10f, bounds.Top - 20f)
                (center - 24f + 10f, bounds.Top - 20f)
                (center - 24f, bounds.Top - 10.0f)
        Render.quad
            moving_average
            config.TimingDisplayMovingAverageColor.AsQuad

    let private create_judgement (config: HudConfig) : SelectPreview =
        let texture = Content.Texture "judgements"
        let display = config.GetJudgementMeterDisplay Rulesets.current
        let dtype = if display.Length > 1 then display.[1] else JudgementDisplayType.Name

        None,
        fun (bounds: Rect) ->

        match dtype with
        | JudgementDisplayType.Name ->
            Text.fill (
                Style.font,
                Rulesets.current.JudgementName 1,
                bounds,
                Rulesets.current.JudgementColor 1,
                Alignment.CENTER
            )
        | JudgementDisplayType.Texture y ->
            Render.tex_quad
                ((Sprite.fill bounds texture).AsQuad)
                Color.White.AsQuad
                (Sprite.pick_texture (1, y) texture)

    let private create_early_late (config: HudConfig) : SelectPreview =
        let texture = Content.Texture "early-late"

        None,
        fun (bounds: Rect) ->

        if config.EarlyLateMeterUseTexture then
            Render.tex_quad
                ((Sprite.fill bounds texture).AsQuad)
                Color.White.AsQuad
                (Sprite.pick_texture (1, 0) texture)
        else
            Text.fill (
                Style.font,
                config.EarlyLateMeterEarlyText,
                bounds,
                config.EarlyLateMeterEarlyColor,
                Alignment.CENTER
            )

    let private create_combo (config: HudConfig) : SelectPreview =
        let texture = Content.Texture "combo-font"

        None,
        fun (bounds: Rect) ->

        if config.ComboUseFont then
            Combo.draw_combo_centered(texture, bounds, Color.White, 727, config.ComboFontSpacing)
        else
            Text.fill (Style.font, "727", bounds, Color.White, Alignment.CENTER)

    let private create_progress_pie (config: HudConfig) : SelectPreview =
        let font_texture = Content.Texture "progress-meter-font"

        None,
        fun (bounds: Rect) ->

        ProgressMeter.draw_pie(bounds.SliceT(bounds.Width), config.ProgressMeterColor, config.ProgressMeterBackgroundColor, 0.6f)

        if config.ProgressMeterUseFont then

            match config.ProgressMeterLabel with
                | ProgressPieLabel.Countdown ->
                    let time_left = 447000.0f<ms / rate>
                    ProgressMeter.draw_countdown_centered (
                        font_texture,
                        bounds.SliceB(bounds.Width * config.ProgressMeterLabelSize),
                        Color.White,
                        time_left,
                        config.ProgressMeterFontSpacing,
                        config.ProgressMeterColonExtraSpacing
                    )
                | ProgressPieLabel.Percentage ->
                    ProgressMeter.draw_percent_progress_centered (
                        font_texture,
                        bounds.SliceB(bounds.Width * config.ProgressMeterLabelSize),
                        Color.White,
                        0.6f,
                        config.ProgressMeterFontSpacing,
                        config.ProgressMeterPercentExtraSpacing
                    )
                | _ -> ()

        else

            let text =
                match config.ProgressMeterLabel with
                | ProgressPieLabel.Countdown -> "7:27"
                | ProgressPieLabel.Percentage -> "60%"
                | _ -> ""

            Text.fill_b (
                Style.font,
                text,
                bounds.SliceB(bounds.Width * config.ProgressMeterLabelSize),
                Colors.text_subheading,
                Alignment.CENTER
            )

    let private create_skip_button (config: HudConfig) : SelectPreview =
        let preview_text = [ (%%"skip").ToString() ] %> "play.skiphint"

        None,
        fun (bounds: Rect) ->
            Text.fill (Style.font, preview_text, bounds, Color.White, Alignment.CENTER)

    let private create_pacemaker (config: HudConfig) : SelectPreview =
        None,
        fun (bounds: Rect) ->
            Text.fill (Style.font, Icons.FLAG, bounds.SliceX(bounds.Height * 2.0f), Color.White, Alignment.CENTER)

    let private create_rate_mods (config: HudConfig) : SelectPreview =
        None,
        fun (bounds: Rect) ->
            Text.fill (Style.font, (if config.RateModMeterShowMods then "1.00x, Mirror" else "1.00x"), bounds, Color.White, Alignment.CENTER)

    let private create_bpm (config: HudConfig) : SelectPreview =
        None,
        fun (bounds: Rect) ->
            Text.fill (Style.font, "120 BPM", bounds, Color.White, Alignment.CENTER)

    let private create_input_meter (config: HudConfig) : SelectPreview =
        let keys = SelectedChart.keymode() |> int

        None,
        fun (bounds: Rect) ->

        let column_width = bounds.Width / float32 keys

        let mutable box =
            (
                if config.InputMeterScrollDownwards then
                    bounds.SliceT(column_width)
                else
                    bounds.SliceB(column_width)
            )
                .SliceL(column_width)
                .ShrinkPercent(config.InputMeterColumnPadding * 0.5f)
        for k = 0 to keys - 1 do
            let key_alpha = float32 config.InputMeterKeyColor.A * (1.0f - 0.5f * float32 k / float32 keys) |> int
            Render.rect box (config.InputMeterKeyColor.O4a key_alpha)
            box <- box.Translate(column_width, 0.0f)

        if config.InputMeterShowInputs then
            let p percentage =
                let y = (bounds.Height - column_width) * percentage
                if config.InputMeterScrollDownwards then
                    bounds.Top + column_width + y
                else
                    bounds.Bottom - column_width - y
            Render.rect
                (
                    Rect.Create(bounds.Left, p 0.0f, bounds.Left + column_width, p 1.0f)
                        .ShrinkPercentX(config.InputMeterColumnPadding * 0.5f)
                )
                config.InputMeterInputColor
            for k = 1 to keys - 1 do
                let p1 = (float32 k / float32 keys) * 0.8f
                Render.rect
                    (
                        Rect.Create(bounds.Left + column_width * float32 k, p p1, bounds.Left + column_width * float32 (k + 1), p (p1 + 0.1f))
                            .ShrinkPercentX(config.InputMeterColumnPadding * 0.5f)
                    )
                    config.InputMeterInputColor

    let private create_kps (config: HudConfig) : SelectPreview =
        None,
        fun (bounds: Rect) ->
            Text.fill (Style.font, "70 KPS", bounds.SlicePercentT 0.5f, Color.White, Alignment.CENTER)

    let private create_custom_image (config: HudConfig) : SelectPreview =
        let texture = Content.Texture "custom-image"

        None,
        fun (bounds: Rect) ->
        Render.tex_quad
            ((Sprite.fill bounds texture).AsQuad)
            Color.White.AsQuad
            (Sprite.pick_texture (1, 0) texture)

    let create (config: HudConfig) (element: HudElement) : SelectPreview =
        match element with
        | HudElement.Accuracy -> create_accuracy config
        | HudElement.ErrorBar -> create_error_bar config
        | HudElement.Judgement -> create_judgement config
        | HudElement.EarlyLate -> create_early_late config
        | HudElement.Combo -> create_combo config
        | HudElement.ProgressPie -> create_progress_pie config
        | HudElement.SkipButton -> create_skip_button config
        | HudElement.Pacemaker -> create_pacemaker config
        | HudElement.JudgementCounter -> None, ignore
        | HudElement.RateMods -> create_rate_mods config
        | HudElement.BPM -> create_bpm config
        | HudElement.InputMeter -> create_input_meter config
        | HudElement.KeysPerSecond -> create_kps config
        | HudElement.CustomImage -> create_custom_image config