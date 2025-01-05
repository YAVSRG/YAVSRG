namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skins.HudLayouts
open Interlude.Content
open Interlude.Features.Play.HUD

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

    let create (config: HudConfig) (element: HudElement) : SelectPreview =
        match element with
        | HudElement.Accuracy -> create_accuracy config
        | HudElement.ErrorBar -> create_error_bar config
        | HudElement.Judgement -> create_judgement config
        | HudElement.EarlyLate -> create_early_late config
        | _ -> None, ignore