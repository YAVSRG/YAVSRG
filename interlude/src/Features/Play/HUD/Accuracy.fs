namespace Interlude.Features.Play.HUD

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay
open Prelude.Content.Noteskins
open Interlude.Features.Play

type Accuracy(user_options: HUDUserOptions, noteskin_options: HUDNoteskinOptions, state: PlayState) as this =
    inherit Container(NodeType.None)

    let grades = state.Ruleset.Grading.Grades

    let color =
        Animation.Color(
            if user_options.AccuracyGradeColors then
                Array.last(grades).Color
            else
                Color.White
        )

    do
        if user_options.AccuracyGradeColors then
            state.SubscribeToHits(fun _ ->
                color.Target <- Grade.calculate grades state.Scoring.State |> state.Ruleset.GradeColor
            )

        this
        |* Text(
            (fun () -> state.Scoring.FormatAccuracy()),
            Color = (fun () -> color.Value, Color.Transparent),
            Align = Alignment.CENTER,
            Position =
                { Position.Default with
                    Bottom = 0.7f %+ 0.0f
                }
        )

        if user_options.AccuracyShowName then
            this
            |* Text(
                (fun () -> state.Scoring.Name),
                Color = K(Color.White, Color.Transparent),
                Align = Alignment.CENTER,
                Position =
                    { Position.Default with
                        Top = 0.6f %+ 0.0f
                    }
            )

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        color.Update elapsed_ms