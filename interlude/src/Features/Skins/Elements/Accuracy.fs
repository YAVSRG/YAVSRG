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

    let draw_accuracy_centered(texture: Sprite, bounds: Rect, color: Color, accuracy: float, spacing: float32, dot_spacing: float32, percent_spacing: float32) =
        let accuracy_text = format_accuracy (max 0.0 accuracy)
        let char_width = float32 texture.Width
        let width = (dot_spacing * 2.0f + percent_spacing + float32 accuracy_text.Length + (float32 accuracy_text.Length - 1.0f) * spacing) * char_width
        let height = float32 texture.Height
        let scale = min (bounds.Width / width) (bounds.Height / height)

        let mutable char_bounds = 
            Rect.Box(
                bounds.CenterX - width * scale * 0.5f,
                bounds.CenterY - height * scale * 0.5f,
                char_width * scale,
                height * scale
            )

        for c in accuracy_text do
            if c = '.' then
                char_bounds <- char_bounds.Translate(scale * dot_spacing * char_width, 0.0f)
                Draw.quad char_bounds.AsQuad color.AsQuad (Sprite.pick_texture (0, 10) texture)
                char_bounds <- char_bounds.Translate(scale * (1.0f + dot_spacing + spacing) * char_width, 0.0f)
            elif c = '%' then
                char_bounds <- char_bounds.Translate(scale * percent_spacing * char_width, 0.0f)
                Draw.quad char_bounds.AsQuad color.AsQuad (Sprite.pick_texture (0, 12) texture)
            else
                Draw.quad char_bounds.AsQuad color.AsQuad (Sprite.pick_texture (0, int (c - '0')) texture)
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

    override this.Init(parent) =
        if config.AccuracyGradeColors then
            state.SubscribeEvents(fun _ ->
                color.Target <- Grade.calculate grades state.Scoring.Accuracy |> state.Ruleset.GradeColor
            )

        
        if not config.AccuracyUseFont then
            this
            |* Text(
                (fun () -> format_accuracy state.Scoring.Accuracy),
                Color = (fun () -> color.Value, Color.Transparent),
                Align = Alignment.CENTER,
                Position =
                    { Position.DEFAULT with
                        Bottom = 0.7f %+ 0.0f
                    }
            )

        if config.AccuracyShowName then
            this
            |* Text(
                K state.Ruleset.Name,
                Color = K Colors.text_subheading,
                Align = Alignment.CENTER,
                Position =
                    { Position.DEFAULT with
                        Top = 0.6f %+ 0.0f
                    }
            )
        base.Init parent

    override this.Draw() =
        base.Draw()
        if config.AccuracyUseFont then
            let text_bounds = this.Bounds.SliceT(this.Bounds.Height * 0.6f)
            Accuracy.draw_accuracy_centered(
                font_texture,
                text_bounds,
                color.Value,
                state.Scoring.Accuracy,
                config.AccuracyFontSpacing,
                config.AccuracyDotExtraSpacing,
                config.AccuracyPercentExtraSpacing
            )

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        color.Update elapsed_ms