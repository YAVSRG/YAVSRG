namespace Interlude.Features.Play.HUD

open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay.Scoring
open Prelude.Skins.HudLayouts
open Interlude.Content
open Interlude.Features.Play

module JudgementCounter =

    let draw_count_aligned (texture: Sprite, bounds: Rect, color: Color, count: int, spacing: float32, alignment: float32) =
        let count_text = count.ToString()
        let char_width = float32 texture.Width

        let width =
            (float32 count_text.Length + (float32 count_text.Length - 1.0f) * spacing)
            * char_width

        let height = float32 texture.Height
        let scale = min (bounds.Width / width) (bounds.Height / height)

        let mutable char_bounds =
            Rect.FromSize(
                bounds.Left + (bounds.Width - width * scale) * alignment,
                bounds.CenterY - height * scale * 0.5f,
                char_width * scale,
                height * scale
            )

        for c in count_text do
            Render.tex_quad char_bounds.AsQuad color.AsQuad (Sprite.pick_texture (0, int (c - '0')) texture)
            char_bounds <- char_bounds.Translate(scale * (1.0f + spacing) * char_width, 0.0f)

    let draw_ratio_centered (texture: Sprite, bounds: Rect, color: Color, (mv, pf): int * int, spacing: float32, dot_spacing: float32, colon_spacing: float32) =
        let ratio_text =
            if pf = 0 then
                sprintf "%.1f:0" (float mv)
            else
                sprintf "%.1f:1" (float mv / float pf)

        let char_width = float32 texture.Width

        let width =
            (dot_spacing * 2.0f
             + colon_spacing * 2.0f
             + float32 ratio_text.Length
             + (float32 ratio_text.Length - 1.0f) * spacing)
            * char_width

        let height = float32 texture.Height
        let scale = min (bounds.Width / width) (bounds.Height / height)

        let mutable char_bounds =
            Rect.FromSize(
                bounds.CenterX - width * scale * 0.5f,
                bounds.CenterY - height * scale * 0.5f,
                char_width * scale,
                height * scale
            )

        for c in ratio_text do
            if c = '.' then
                char_bounds <- char_bounds.Translate(scale * dot_spacing * char_width, 0.0f)
                Render.tex_quad char_bounds.AsQuad color.AsQuad (Sprite.pick_texture (0, 10) texture)
                char_bounds <- char_bounds.Translate(scale * (1.0f + dot_spacing + spacing) * char_width, 0.0f)
            elif c = ':' then
                char_bounds <- char_bounds.Translate(scale * colon_spacing * char_width, 0.0f)
                Render.tex_quad char_bounds.AsQuad color.AsQuad (Sprite.pick_texture (0, 11) texture)
                char_bounds <- char_bounds.Translate(scale * (1.0f + colon_spacing + spacing) * char_width, 0.0f)
            else
                Render.tex_quad char_bounds.AsQuad color.AsQuad (Sprite.pick_texture (0, int (c - '0')) texture)
                char_bounds <- char_bounds.Translate(scale * (1.0f + spacing) * char_width, 0.0f)

type JudgementCounter(config: HudConfig, state: PlayState) =
    inherit Container(NodeType.None)

    let judgement_animations =
        Array.init state.Ruleset.Judgements.Length (fun _ -> Animation.Delay(float config.JudgementCounterFadeTime))

    let texture = Content.Texture "judgement-counter-judgements"
    let display: int option array = config.GetJudgementCounterDisplay state.Ruleset

    let font = Content.Texture "judgement-counter-font"

    let opacity = 255f * config.JudgementCounterOpacity |> int |> max 0 |> min 255

    let count_alignment =
        if config.JudgementCounterShowLabels then
            Alignment.RIGHT
        else
            Alignment.CENTER

    override this.Init(parent) =
        state.SubscribeEvents(fun h ->
            match h.Action.Judgement with
            | Some(j, _) -> judgement_animations[j].Reset()
            | None -> ()
        )

        base.Init parent

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        for j in judgement_animations do
            j.Update elapsed_ms

    override this.Draw() =
        base.Draw()

        let h =
            this.Bounds.Height
            / float32 (judgement_animations.Length + if config.JudgementCounterShowRatio then 1 else 0)
            |> min this.Bounds.Width

        let mutable r = this.Bounds.SliceT(h).Shrink(10.0f)

        for i = 0 to state.Ruleset.Judgements.Length - 1 do
            let j = state.Ruleset.Judgements.[i]

            let percent = float32 judgement_animations.[i].Progress

            let pop =
                r.Expand(r.Height * (1.0f - percent) * config.JudgementCounterPopAmount * 0.1f)

            Render.border Style.PADDING pop (j.Color.O3a opacity)
            Render.rect (pop.BorderL(5.0f).SlicePercentY(0.45f)) j.Color.O2
            Render.rect pop (j.Color.O1a opacity)

            if config.JudgementCounterShowLabels then

                match display.[i] with
                | Some texture_index ->
                    Render.tex_quad
                        ((Sprite.fill_left (pop.SlicePercentY(config.JudgementCounterTextScale).ShrinkX(10.0f)) texture).AsQuad)
                        Color.White.AsQuad
                        (Sprite.pick_texture (0, texture_index) texture)
                | None ->
                    Text.fill_b (
                        Style.font,
                        j.Name,
                        pop.SlicePercentY(config.JudgementCounterTextScale / 0.6f).ShrinkX(10.0f),
                        (Color.White, Color.Black),
                        Alignment.LEFT
                    )

            if config.JudgementCounterUseFont then
                JudgementCounter.draw_count_aligned (
                    font,
                    (
                        if config.JudgementCounterShowLabels then
                            pop.SlicePercentY(config.JudgementCounterTextScale).ShrinkX(10.0f)
                        else
                            pop.SlicePercentY(config.JudgementCounterTextScale).ExpandPercentX(2.0f)
                    ),
                    Color.White,
                    state.Scoring.JudgementCounts.[i],
                    config.JudgementCounterFontSpacing,
                    count_alignment
                )
            else
                Text.fill_b (
                    Style.font,
                    state.Scoring.JudgementCounts.[i].ToString(),
                    (
                        if config.JudgementCounterShowLabels then
                            pop.SlicePercentY(config.JudgementCounterTextScale / 0.6f).ShrinkX(10.0f)
                        else
                            pop.SlicePercentY(config.JudgementCounterTextScale / 0.6f).ExpandPercentX(2.0f)
                    ),
                    (Color.White, Color.Black),
                    count_alignment
                )

            r <- r.Translate(0.0f, h)

        if config.JudgementCounterShowRatio && state.Scoring.JudgementCounts.Length > 1 then
            let ratio = state.Scoring.JudgementCounts.[0], state.Scoring.JudgementCounts.[1]

            if config.JudgementCounterUseFont then
                JudgementCounter.draw_ratio_centered (
                    font,
                    r.SlicePercentT(config.JudgementCounterTextScale).ExpandPercentX(2.0f),
                    Color.White,
                    ratio,
                    config.JudgementCounterFontSpacing,
                    config.JudgementCounterDotExtraSpacing,
                    config.JudgementCounterColonExtraSpacing
                )
            else
                let (mv, pf) = ratio

                Text.fill_b (
                    Style.font,
                    (if pf = 0 then
                         sprintf "%.1f:0" (float mv)
                     else
                         sprintf "%.1f:1" (float mv / float pf)),
                    r.SlicePercentT(config.JudgementCounterTextScale / 0.6f).ExpandPercentX(2.0f),
                    (Color.White, Color.Black),
                    Alignment.CENTER
                )