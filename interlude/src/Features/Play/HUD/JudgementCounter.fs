namespace Interlude.Features.Play.HUD

open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay
open Prelude.Content.Noteskins
open Interlude.Content
open Interlude.Features.Play

module JudgementCounter =

    let draw_count_right_aligned(texture: Sprite, bounds: Rect, color: Color, count: int, spacing: float32) =
        let count_text = count.ToString()
        let char_width = float32 texture.Width
        let width = (float32 count_text.Length + (float32 count_text.Length - 1.0f) * spacing) * char_width
        let height = float32 texture.Height
        let scale = min (bounds.Width / width) (bounds.Height / height)

        let mutable char_bounds = 
            Rect.Box(
                bounds.Right - width * scale,
                bounds.CenterY - height * scale * 0.5f,
                char_width * scale,
                height * scale
            )

        for c in count_text do
            Draw.quad char_bounds.AsQuad color.AsQuad (Sprite.pick_texture (0, int (c - '0')) texture)
            char_bounds <- char_bounds.Translate(scale * (1.0f + spacing) * char_width, 0.0f)

    let draw_ratio_centered(texture: Sprite, bounds: Rect, color: Color, (mv, pf) : int * int, spacing: float32, dot_spacing: float32, colon_spacing: float32) =
        let ratio_text = sprintf "%.2f:1" (float mv / float pf)
        let char_width = float32 texture.Width
        let width = (dot_spacing * 2.0f + colon_spacing * 2.0f + float32 ratio_text.Length + (float32 ratio_text.Length - 1.0f) * spacing) * char_width
        let height = float32 texture.Height
        let scale = min (bounds.Width / width) (bounds.Height / height)

        let mutable char_bounds = 
            Rect.Box(
                bounds.CenterX - width * scale * 0.5f,
                bounds.CenterY - height * scale * 0.5f,
                char_width * scale,
                height * scale
            )

        for c in ratio_text do
            if c = '.' then
                char_bounds <- char_bounds.Translate(scale * dot_spacing * char_width, 0.0f)
                Draw.quad char_bounds.AsQuad color.AsQuad (Sprite.pick_texture (0, 10) texture)
                char_bounds <- char_bounds.Translate(scale * (1.0f + dot_spacing + spacing) * char_width, 0.0f)
            elif c = ':' then
                char_bounds <- char_bounds.Translate(scale * colon_spacing * char_width, 0.0f)
                Draw.quad char_bounds.AsQuad color.AsQuad (Sprite.pick_texture (0, 11) texture)
                char_bounds <- char_bounds.Translate(scale * (1.0f + colon_spacing + spacing) * char_width, 0.0f)
            else
                Draw.quad char_bounds.AsQuad color.AsQuad (Sprite.pick_texture (0, int (c - '0')) texture)
                char_bounds <- char_bounds.Translate(scale * (1.0f + spacing) * char_width, 0.0f)
        
type JudgementCounter(user_options: HUDUserOptions, noteskin_options: HUDNoteskinOptions, state: PlayState) =
    inherit Container(NodeType.None)

    let judgement_animations =
        Array.init state.Ruleset.Judgements.Length (fun _ -> Animation.Delay(user_options.JudgementCounterFadeTime))

    let texture = Content.Texture "judgement-counter-judgements"
    let display : int option array = noteskin_options.GetJudgementCounterDisplay state.Ruleset
    let font = Content.Texture "judgement-counter-font"

    override this.Init(parent) =
        state.SubscribeToHits(fun h ->
            match h.Guts with
            | Hit x ->
                if x.Judgement.IsSome then
                    judgement_animations[x.Judgement.Value].Reset()
            | Release x ->
                if x.Judgement.IsSome then
                    judgement_animations[x.Judgement.Value].Reset()
        )

        let background = noteskin_options.JudgementCounterBackground
        if background.Enable then
            let lo = (1.0f - background.Scale) * 0.5f
            let hi = 1.0f - lo
            this 
            |* Image(
                Content.Texture "judgement-counter-bg",
                StretchToFill = false,
                Position = 
                    { 
                        Left = (lo - 0.5f + background.AlignmentX) %+ 0.0f
                        Top = (lo - 0.5f + background.AlignmentY) %+ 0.0f
                        Right = (hi - 0.5f + background.AlignmentX) %+ 0.0f
                        Bottom = (hi - 0.5f + background.AlignmentY) %+ 0.0f
                    }
            )
        base.Init parent

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        for j in judgement_animations do
            j.Update elapsed_ms

    override this.Draw() =
        base.Draw()
        let h = this.Bounds.Height / float32 (judgement_animations.Length + if user_options.JudgementCounterShowRatio then 1 else 0)
        let mutable r = this.Bounds.SliceTop(h)

        for i = 0 to state.Ruleset.Judgements.Length - 1 do
            let j = state.Ruleset.Judgements.[i]

            if not judgement_animations.[i].Complete && state.Scoring.State.Judgements.[i] > 0 then
                Draw.rect
                    r
                    (Color.FromArgb(
                        127
                        - max 0 (int (127.0 * judgement_animations.[i].Elapsed / judgement_animations.[i].Interval)),
                        j.Color
                    ))

            match display.[i] with
            | Some texture_index ->
                Draw.quad 
                        ((Sprite.fill_left (r.Shrink(5.0f)) texture).AsQuad)
                        Color.White.AsQuad
                        (Sprite.pick_texture (0, texture_index) texture)
            | None ->
                Draw.rect (r.SliceLeft(5.0f)) j.Color
                Text.fill_b (Style.font, j.Name, r.Shrink(5.0f), (Color.White, Color.Black), Alignment.LEFT)

            if noteskin_options.JudgementCounterUseFont then
                JudgementCounter.draw_count_right_aligned(font, r.Shrink(5.0f), Color.White, state.Scoring.State.Judgements.[i], noteskin_options.JudgementCounterFontSpacing)
            else
                Text.fill_b (
                    Style.font,
                    state.Scoring.State.Judgements.[i].ToString(),
                    r.Shrink(5.0f),
                    (Color.White, Color.Black),
                    Alignment.RIGHT
                )

            r <- r.Translate(0.0f, h)

        if user_options.JudgementCounterShowRatio && state.Scoring.State.Judgements.Length > 1 then
            let ratio = state.Scoring.State.Judgements.[0], state.Scoring.State.Judgements.[1]
            if noteskin_options.JudgementCounterUseFont then
                JudgementCounter.draw_ratio_centered(
                    font,
                    r.Shrink(5.0f),
                    Color.White,
                    ratio,
                    noteskin_options.JudgementCounterFontSpacing,
                    noteskin_options.JudgementCounterDotExtraSpacing,
                    noteskin_options.JudgementCounterColonExtraSpacing
                )
            else
                let (mv, pf) = ratio
                Text.fill_b (
                    Style.font,
                    sprintf "%.2f:1" (float mv / float pf),
                    r.Shrink(5.0f),
                    (Color.White, Color.Black),
                    Alignment.CENTER
                )