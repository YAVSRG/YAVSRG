namespace Interlude.Features.Play.HUD

open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay
open Prelude.Content.Noteskins
open Interlude.Content
open Interlude.Features.Play

type JudgementCounter(user_options: HUDUserOptions, noteskin_options: HUDNoteskinOptions, state: PlayState) =
    inherit Container(NodeType.None)

    let judgement_animations =
        Array.init state.Ruleset.Judgements.Length (fun _ -> Animation.Delay(user_options.JudgementCounterFadeTime))

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

        if noteskin_options.JudgementCounterUseBackground then
            let lo = (1.0f - noteskin_options.JudgementCounterBackgroundScale) * 0.5f
            let hi = 1.0f - lo
            this 
            |* Image(
                Content.Texture "judgement-counter-bg",
                StretchToFill = false,
                Position = 
                    { 
                        Left = lo %+ 0.0f
                        Top = lo %+ 0.0f
                        Right = hi %+ 0.0f
                        Bottom = hi %+ 0.0f
                    }
            )
        base.Init parent

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        for j in judgement_animations do
            j.Update elapsed_ms

    override this.Draw() =
        base.Draw()
        let h = this.Bounds.Height / float32 judgement_animations.Length
        let mutable r = this.Bounds.SliceTop(h).Shrink(5.0f)

        for i = 0 to state.Ruleset.Judgements.Length - 1 do
            let j = state.Ruleset.Judgements.[i]
            Draw.rect (r.Expand(10.0f, 5.0f).SliceLeft(5.0f)) j.Color

            if not judgement_animations.[i].Complete && state.Scoring.State.Judgements.[i] > 0 then
                Draw.rect
                    (r.Expand 5.0f)
                    (Color.FromArgb(
                        127
                        - max 0 (int (127.0 * judgement_animations.[i].Elapsed / judgement_animations.[i].Interval)),
                        j.Color
                    ))

            Text.fill_b (Style.font, j.Name, r, (Color.White, Color.Black), Alignment.LEFT)

            Text.fill_b (
                Style.font,
                state.Scoring.State.Judgements.[i].ToString(),
                r,
                (Color.White, Color.Black),
                Alignment.RIGHT
            )

            r <- r.Translate(0.0f, h)