namespace Interlude.Features.Play.HUD

open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay
open Prelude.Content.Noteskins
open Interlude.UI
open Interlude.Content
open Interlude.Features.Play
open Interlude.Utils

type Pacemaker(user_options: HUDUserOptions, noteskin_options: HUDNoteskinOptions, state: PlayState) =
    inherit StaticWidget(NodeType.None)

    let color = Animation.Color(Color.White)
    let flag_position = Animation.Fade(0.5f)
    let position_cooldown = Animation.Delay(3000.0)
    let mutable ahead_by = 0.0
    let mutable hearts = -1

    let update_flag_position () =
        if ahead_by >= 10.0 then
            flag_position.Target <- 1.0f
        elif ahead_by > -10.0 then
            flag_position.Target <- (ahead_by + 10.0) / 20.0 |> float32

            if ahead_by > 0.0 then
                color.Target <- Color.FromHsv(140.0f / 360.0f, ahead_by / 10.0 |> float32, 1.0f)
            else
                color.Target <- Color.FromHsv(340.0f / 360.0f, ahead_by / -10.0 |> float32, 1.0f)
        else
            flag_position.Target <- 0.0f

    do
        match state.Pacemaker with
        | PacemakerInfo.None
        | PacemakerInfo.Accuracy _
        | PacemakerInfo.Replay _ -> ()
        | PacemakerInfo.Judgement(judgement, _) ->
            color.Target <-
                if judgement = -1 then
                    Rulesets.current.Judgements.[Rulesets.current.Judgements.Length - 1].Color
                else
                    Rulesets.current.Judgements.[judgement].Color

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        match state.Pacemaker with
        | PacemakerInfo.None -> ()
        | PacemakerInfo.Accuracy x ->
            if position_cooldown.Complete then
                ahead_by <- state.Scoring.State.PointsScored - state.Scoring.State.MaxPointsScored * x
                update_flag_position ()
                position_cooldown.Reset()

            flag_position.Update elapsed_ms
            position_cooldown.Update elapsed_ms
        | PacemakerInfo.Replay score ->
            if position_cooldown.Complete then
                score.Update(state.CurrentChartTime())
                ahead_by <- state.Scoring.State.PointsScored - score.State.PointsScored
                update_flag_position ()
                position_cooldown.Reset()

            flag_position.Update elapsed_ms
            position_cooldown.Update elapsed_ms
        | PacemakerInfo.Judgement(_, _) -> ()

        color.Update elapsed_ms

    override this.Draw() =
        match state.Pacemaker with
        | PacemakerInfo.None ->
            Text.fill_b (
                Style.font,
                Icons.FLAG,
                this.Bounds
                    .SliceLeft(0.0f)
                    .Expand(this.Bounds.Height, 0.0f)
                    .Translate(this.Bounds.Width * 0.5f, 0.0f),
                (color.Value, Color.Black),
                Alignment.CENTER
            )
        | PacemakerInfo.Accuracy _
        | PacemakerInfo.Replay _ ->
            Text.fill_b (
                Style.font,
                Icons.FLAG,
                this.Bounds
                    .SliceLeft(0.0f)
                    .Expand(this.Bounds.Height, 0.0f)
                    .Translate(this.Bounds.Width * flag_position.Value, 0.0f),
                (color.Value, Color.Black),
                Alignment.CENTER
            )
        | PacemakerInfo.Judgement(judgement, count) ->
            let actual =
                if judgement = -1 then
                    state.Scoring.State.ComboBreaks
                else
                    let mutable c = state.Scoring.State.Judgements.[judgement]

                    for j = judgement + 1 to state.Scoring.State.Judgements.Length - 1 do
                        if state.Scoring.State.Judgements.[j] > 0 then
                            c <- 1000000

                    c

            let _hearts = 1 + count - actual

            if _hearts < hearts then
                color.Value <- Color.White

            hearts <- _hearts

            let display =
                if hearts > 5 then
                    sprintf "%s x%i" (String.replicate 5 Icons.HEART_ON) hearts
                elif hearts > 0 then
                    (String.replicate hearts Icons.HEART_ON)
                else
                    Icons.X

            Text.fill_b (Style.font, display, this.Bounds, (color.Value, Color.Black), Alignment.CENTER)