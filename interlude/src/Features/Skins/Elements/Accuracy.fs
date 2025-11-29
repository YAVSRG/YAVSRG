namespace Interlude.Features.Play.HUD

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

type Accuracy(ctx: HudContext) =
    inherit Container(NodeType.None)

    let color =
        Animation.Color(
            if ctx.Config.AccuracyGradeColors then
                ctx.State.Ruleset.GradeColor (ctx.State.Ruleset.Grades.Length - 1)
            else
                Color.White
        )

    let font_texture = Content.Texture "accuracy-font"

    let alignment = ctx.Config.AccuracyPosition.TextAlignment

    override this.Init(parent) =
        if ctx.Config.AccuracyGradeColors then
            ctx.State.Subscribe(fun _ ->
                color.Target <- Grade.calculate ctx.State.Ruleset.Grades ctx.State.Scoring.Accuracy |> ctx.State.Ruleset.GradeColor
            ) |> ignore

        if not ctx.Config.AccuracyUseFont then
            this
            |* Text(fun () -> ctx.State.Scoring.FormattedAccuracy)
                .Color(fun () -> color.Value, Color.Transparent)
                .Align(alignment)
                .Position(Position.SlicePercentT(0.7f))

        if ctx.Config.AccuracyShowName then
            this
            |* Text(ctx.State.Ruleset.Name)
                .Color(Colors.text_subheading)
                .Align(alignment)
                .Position(Position.SlicePercentB(0.4f))
        base.Init parent

    override this.Draw() =
        base.Draw()
        if ctx.Config.AccuracyUseFont then
            let text_bounds = this.Bounds.SliceT(this.Bounds.Height * 0.6f)
            Accuracy.draw_accuracy_aligned(
                font_texture,
                text_bounds,
                color.Value,
                ctx.State.Scoring.FormattedAccuracy,
                ctx.Config.AccuracyFontSpacing,
                ctx.Config.AccuracyDotExtraSpacing,
                ctx.Config.AccuracyPercentExtraSpacing,
                alignment
            )

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        color.Update elapsed_ms