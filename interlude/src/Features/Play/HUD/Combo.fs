namespace Interlude.Features.Play.HUD

open System
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay
open Prelude.Content.Noteskins
open Interlude.Content
open Interlude.Features.Play

module Combo =
    
    let draw_noteskin_font(texture: Sprite, bounds: Rect, color: Color, combo: int, spacing: float32) =
        let combo_text = combo.ToString()
        let char_width = float32 texture.Width
        let width = (float32 combo_text.Length + (float32 combo_text.Length - 1.0f) * spacing) * char_width
        let height = float32 texture.Height
        let scale = min (bounds.Width / width) (bounds.Height / height)

        let mutable char_bounds = 
            Rect.Box(
                bounds.CenterX - width * scale * 0.5f,
                bounds.CenterY - height * scale * 0.5f,
                char_width * scale,
                height * scale
            )

        for c in combo_text do
            Draw.quad char_bounds.AsQuad color.AsQuad (Sprite.pick_texture (0, int (c - '0')) texture)
            char_bounds <- char_bounds.Translate(scale * (1.0f + spacing) * char_width, 0.0f)

type Combo(user_options: HUDUserOptions, noteskin_options: HUDNoteskinOptions, state: PlayState) =
    inherit StaticWidget(NodeType.None)
    let pop_animation = Animation.Fade(0.0f)
    let color = Animation.Color(Color.White)
    let mutable hits = 0

    let font_texture = Content.Texture "combo-font"

    do
        state.SubscribeToHits(fun _ ->
            hits <- hits + 1

            if (user_options.ComboLampColors && hits > 50) then
                color.Target <-
                    Lamp.calculate state.Ruleset.Grading.Lamps state.Scoring.State
                    |> state.Ruleset.LampColor

            pop_animation.Value <- noteskin_options.ComboPop
        )

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        color.Update elapsed_ms
        pop_animation.Update elapsed_ms

    override this.Draw() =
        let combo = state.Scoring.State.CurrentCombo

        let amt =
            pop_animation.Value
            + (((combo, 1000) |> Math.Min |> float32) * noteskin_options.ComboGrowth)

        let bounds = this.Bounds.Expand amt

        if noteskin_options.ComboUseFont then
            Combo.draw_noteskin_font(font_texture, bounds, color.Value, combo, noteskin_options.ComboFontSpacing)
        else
            Text.fill (Style.font, combo.ToString(), this.Bounds.Expand amt, color.Value, 0.5f)