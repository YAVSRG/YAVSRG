namespace Interlude.Features.Play.HUD

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay.Scoring
open Prelude.Skins.HudLayouts
open Interlude.Content
open Interlude.Features.Play

module Accuracy =

    let draw_accuracy_aligned(texture: Sprite, bounds: Rect, color: Color, accuracy_text: string, spacing: float32, dot_spacing: float32, percent_spacing: float32, alignment: float32) =
        let char_width = float32 texture.Width
        let width = (dot_spacing * 2.0f + percent_spacing + float32 accuracy_text.Length + (float32 accuracy_text.Length - 1.0f) * spacing) * char_width
        let height = float32 texture.Height
        let scale = min (bounds.Width / width) (bounds.Height / height)

        let mutable char_bounds =
            Rect.FromSize(
                bounds.Left + (bounds.Width - width * scale) * alignment,
                bounds.CenterY - height * scale * 0.5f,
                char_width * scale,
                height * scale
            )

        for c in accuracy_text do
            if c = '.' then
                char_bounds <- char_bounds.Translate(scale * dot_spacing * char_width, 0.0f)
                Render.tex_quad char_bounds.AsQuad color.AsQuad (Sprite.pick_texture (0, 10) texture)
                char_bounds <- char_bounds.Translate(scale * (1.0f + dot_spacing + spacing) * char_width, 0.0f)
            elif c = '%' then
                char_bounds <- char_bounds.Translate(scale * percent_spacing * char_width, 0.0f)
                Render.tex_quad char_bounds.AsQuad color.AsQuad (Sprite.pick_texture (0, 12) texture)
            else
                Render.tex_quad char_bounds.AsQuad color.AsQuad (Sprite.pick_texture (0, int (c - '0')) texture)
                char_bounds <- char_bounds.Translate(scale * (1.0f + spacing) * char_width, 0.0f)

type Accuracy(config: HudConfig, state: PlayState) =
    inherit Container(NodeType.None)

    let grades = state.Ruleset.Grades

    let color =
        Animation.Color(
            if config.AccuracyGradeColors then
                Array.last(grades).Color
            else
                Color.White
        )

    let font_texture = Content.Texture "accuracy-font"

    let alignment = config.AccuracyPosition.TextAlignment

    override this.Init(parent) =
        if config.AccuracyGradeColors then
            state.SubscribeEvents(fun _ ->
                color.Target <- Grade.calculate grades state.Scoring.Accuracy |> state.Ruleset.GradeColor
            )

        if not config.AccuracyUseFont then
            this
            |* Text(fun () -> state.Scoring.FormattedAccuracy)
                .Color(fun () -> color.Value, Color.Transparent)
                .Align(alignment)
                .Position(Position.SlicePercentT(0.7f))

        if config.AccuracyShowName then
            this
            |* Text(state.Ruleset.Name)
                .Color(Colors.text_subheading)
                .Align(alignment)
                .Position(Position.SlicePercentB(0.4f))
        base.Init parent

    override this.Draw() =
        base.Draw()
        if config.AccuracyUseFont then
            let text_bounds = this.Bounds.SliceT(this.Bounds.Height * 0.6f)
            Accuracy.draw_accuracy_aligned(
                font_texture,
                text_bounds,
                color.Value,
                state.Scoring.FormattedAccuracy,
                config.AccuracyFontSpacing,
                config.AccuracyDotExtraSpacing,
                config.AccuracyPercentExtraSpacing,
                alignment
            )

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        color.Update elapsed_ms