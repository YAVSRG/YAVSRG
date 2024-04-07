namespace Interlude.Features.Play.HUD

open System
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay
open Prelude.Content.Noteskins
open Interlude.Features.Play

type Combo(user_options: HUDUserOptions, noteskin_options: HUDNoteskinOptions, state: PlayState) =
    inherit StaticWidget(NodeType.None)
    let pop_animation = Animation.Fade(0.0f)
    let color = Animation.Color(Color.White)
    let mutable hits = 0

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

        Text.fill (Style.font, combo.ToString(), this.Bounds.Expand amt, color.Value, 0.5f)