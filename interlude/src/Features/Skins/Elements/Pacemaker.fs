namespace Interlude.Features.Play.HUD

open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skins.HudLayouts
open Interlude.UI
open Interlude.Content
open Interlude.Features.Play
open Interlude.Features.Pacemaker

type Pacemaker(config: HudConfig, state: PlayState) =
    inherit StaticWidget(NodeType.None)

    let AHEAD_BY_SCALE = 15.0

    let color = Animation.Color(Color.White)
    let flag_position = Animation.Fade(0.5f)
    let position_cooldown = Animation.Delay(250.0)
    let mutable ahead_by = 0.0
    let mutable time_last_frame = 0.0f<ms>
    let mutable hearts = -1

    let update_flag_position () =
        if ahead_by >= AHEAD_BY_SCALE then
            flag_position.Target <- 1.0f
        elif ahead_by > -AHEAD_BY_SCALE then
            flag_position.Target <- (ahead_by + AHEAD_BY_SCALE) / AHEAD_BY_SCALE / 2.0 |> float32
        else
            flag_position.Target <- 0.0f

        if ahead_by > 0.0 then
            color.Target <- Color.FromHsv(140.0f / 360.0f, ahead_by / AHEAD_BY_SCALE |> float32 |> min 1.0f, 1.0f)
        else
            color.Target <- Color.FromHsv(340.0f / 360.0f, ahead_by / -AHEAD_BY_SCALE |> float32 |> min 1.0f, 1.0f)

    do
        match state.Pacemaker with
        | PacemakerState.None
        | PacemakerState.Accuracy _
        | PacemakerState.Replay _ -> ()
        | PacemakerState.Judgement(judgement, _) ->
            color.Target <- Rulesets.current.Judgements.[judgement].Color
        | PacemakerState.ComboBreaks _ ->
            color.Target <- Rulesets.current.Judgements.[Rulesets.current.Judgements.Length - 1].Color

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        match state.Pacemaker with
        | PacemakerState.None -> ()
        | PacemakerState.Accuracy x ->
            if position_cooldown.Complete then
                ahead_by <- state.Scoring.PointsScored - state.Scoring.MaxPossiblePoints * x
                update_flag_position ()
                position_cooldown.Reset()

            flag_position.Update elapsed_ms
            position_cooldown.Update elapsed_ms
        | PacemakerState.Replay (_, opposing_score) ->
            if position_cooldown.Complete then
                opposing_score.Update(time_last_frame)
                ahead_by <- state.Scoring.PointsScored - opposing_score.PointsScored
                update_flag_position ()
                position_cooldown.Reset()
            time_last_frame <- state.CurrentChartTime()

            flag_position.Update elapsed_ms
            position_cooldown.Update elapsed_ms
        | PacemakerState.Judgement _
        | PacemakerState.ComboBreaks _ -> ()

        color.Update elapsed_ms

    override this.Draw() =
        match state.Pacemaker with
        | PacemakerState.None ->
            Text.fill_b (
                Style.font,
                Icons.FLAG,
                this.Bounds
                    .SliceL(0.0f)
                    .Expand(this.Bounds.Height, 0.0f)
                    .Translate(this.Bounds.Width * 0.5f, 0.0f),
                (color.Value, Color.Black),
                Alignment.CENTER
            )
        | PacemakerState.Accuracy _
        | PacemakerState.Replay _ ->
            Text.fill_b (
                Style.font,
                Icons.FLAG,
                this.Bounds
                    .SliceL(0.0f)
                    .Expand(this.Bounds.Height, 0.0f)
                    .Translate(this.Bounds.Width * flag_position.Value, 0.0f),
                (color.Value, Color.Black),
                Alignment.CENTER
            )
        | PacemakerState.Judgement(judgement, count) ->
            let actual =
                let mutable c = state.Scoring.JudgementCounts.[judgement]

                for j = judgement + 1 to state.Scoring.JudgementCounts.Length - 1 do
                    if state.Scoring.JudgementCounts.[j] > 0 then
                        c <- 1000000

                c

            let _hearts = 1 + count - actual

            if _hearts < hearts then
                color.Value <- color.Value.O0

            hearts <- _hearts

            let display =
                if hearts > 5 then
                    sprintf "%s x%i" (String.replicate 5 Icons.HEART_ON) hearts
                elif hearts > 0 then
                    (String.replicate hearts Icons.HEART_ON)
                else
                    Icons.X

            Text.fill_b (Style.font, display, this.Bounds, (color.Value, Color.Black), Alignment.CENTER)
        | PacemakerState.ComboBreaks count ->
            let _hearts = 1 + count - state.Scoring.ComboBreaks

            if _hearts < hearts then
                color.Value <- color.Value.O0

            hearts <- _hearts

            let display =
                if hearts > 5 then
                    sprintf "%s x%i" (String.replicate 5 Icons.HEART_ON) hearts
                elif hearts > 0 then
                    (String.replicate hearts Icons.HEART_ON)
                else
                    Icons.X

            Text.fill_b (Style.font, display, this.Bounds, (color.Value, Color.Black), Alignment.CENTER)