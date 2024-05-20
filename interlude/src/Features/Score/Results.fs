namespace Interlude.Features.Score

open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude
open Prelude.Gameplay
open Prelude.Data
open Interlude.UI

#nowarn "3370"

type Grade(grade: Grade.GradeResult ref, score_info: ScoreInfo) =
    inherit Container(NodeType.None)

    override this.Init(parent) =
        this
        |* Text(
            (fun () -> score_info.Ruleset.GradeName (!grade).Grade),
            Color = (fun () -> (score_info.Ruleset.GradeColor (!grade).Grade, Colors.black)),
            Position = Position.Margin(-10.0f)
        )

        base.Init parent

    override this.Draw() =
        Draw.rect (this.Bounds.Translate(10.0f, 10.0f)) Colors.black
        Background.draw (this.Bounds, (Color.FromArgb(40, 40, 40)), 2.0f)
        let grade_color = score_info.Ruleset.GradeColor (!grade).Grade
        Draw.rect this.Bounds grade_color.O1
        base.Draw()

type Accuracy
    (
        grade: Grade.GradeResult ref,
        improvements: ImprovementFlags ref,
        previous_personal_bests: Bests option ref,
        score_info: ScoreInfo
    ) =
    inherit Container(NodeType.None)

    let LOWER_SIZE = 40.0f
    let new_record = sprintf "%s %s" Icons.AWARD (%"score.new_record")
    let mutable hover = false

    let glint_animation = Animation.Delay(375.0)

    override this.Init(parent) =
        this
        |* Text(
            (fun () -> score_info.Scoring.FormatAccuracy()),
            Color = (fun () -> (score_info.Ruleset.GradeColor (!grade).Grade, Colors.black)),
            Position = Position.Margin(10.0f, 0.0f).TrimBottom(LOWER_SIZE)
        )

        if (!improvements).Accuracy <> Improvement.None then ScoreScreenHelpers.animation_queue.Add glint_animation
        base.Init parent


    override this.Update(elapsed_ms, moved) =
        hover <- Mouse.hover this.Bounds
        base.Update(elapsed_ms, moved)

    override this.Draw() =
        Draw.rect (this.Bounds.Translate(10.0f, 10.0f)) Colors.black
        Background.draw (this.Bounds, (Color.FromArgb(40, 40, 40)), 2.0f)
        let grade_color = score_info.Ruleset.GradeColor (!grade).Grade
        Draw.rect (this.Bounds.TrimBottom(LOWER_SIZE)) grade_color.O1
        Draw.rect (this.Bounds.SliceBottom(LOWER_SIZE)) grade_color.O2

        Glint.draw (float32 (glint_animation.Elapsed / glint_animation.Interval)) this.Bounds

        Text.fill_b (
            Style.font,
            score_info.Scoring.FormatAccuracy(),
            this.Bounds.Shrink(10.0f, 0.0f).TrimBottom(LOWER_SIZE),
            (grade_color, Colors.black),
            Alignment.CENTER
        )

        let text, color =
            match (!improvements).Accuracy with
            | Improvement.New -> new_record, Colors.text_yellow_2
            | Improvement.Faster r -> sprintf "%s  •  +%gx" new_record (System.MathF.Round(r, 2)), Colors.text_cyan_2
            | Improvement.Better b -> sprintf "%s  •  +%.2f%%" new_record (b * 100.0), Colors.text_green_2
            | Improvement.FasterBetter(r, b) ->
                sprintf "%s  •  +%.2f%%  •  +%gx" new_record (b * 100.0) (System.MathF.Round(r, 2)), Colors.text_pink_2
            | Improvement.None ->
                match (!previous_personal_bests) with
                | Some pbs ->
                    match PersonalBests.get_best_above_with_rate score_info.Rate pbs.Accuracy with
                    | Some(v, r) ->

                        let summary, distance_from_pb =
                            if r > score_info.Rate then
                                sprintf "%.2f%% (%.2fx)" (v * 100.0) r, (v - score_info.Scoring.Value)
                            else
                                sprintf "%.2f%%" (v * 100.0), (v - score_info.Scoring.Value)

                        if distance_from_pb < 0.0001 then
                            [summary] %> "score.your_record", (Colors.grey_2.O2, Colors.black)
                        else
                            [sprintf "%.2f%%" (distance_from_pb * 100.0); summary] %> "score.compare_accuracy",
                            (Colors.grey_2.O2, Colors.black)

                    | None -> "--", (Colors.grey_2.O2, Colors.black)
                | None -> "--", (Colors.grey_2.O2, Colors.black)

        Text.fill_b (Style.font, text, this.Bounds.Shrink(10.0f, 0.0f).SliceBottom(LOWER_SIZE), color, Alignment.CENTER)
        base.Draw()

        if hover then
            let acc_tooltip = this.Bounds.Expand(-130.0f, 75.0f).SliceBottom(60.0f)
            Draw.rect acc_tooltip Colors.shadow_2.O2

            Text.fill_b (
                Style.font,
                sprintf "%.4f%%" (score_info.Scoring.Value * 100.0),
                acc_tooltip.Shrink(10.0f, 5.0f),
                Colors.text,
                Alignment.CENTER
            )

type Lamp
    (
        lamp: Lamp.LampResult ref,
        improvements: ImprovementFlags ref,
        previous_personal_bests: Bests option ref,
        score_info: ScoreInfo
    ) =
    inherit Container(NodeType.None)

    let LOWER_SIZE = 40.0f
    let new_record = sprintf "%s %s" Icons.AWARD (%"score.new_record")

    let glint_animation = Animation.Delay(375.0)

    override this.Init(parent) =
        this
        |* Text(
            (fun () -> score_info.Ruleset.LampName (!lamp).Lamp),
            Color = (fun () -> (score_info.Ruleset.LampColor (!lamp).Lamp, Colors.black)),
            Position = Position.Margin(10.0f, 0.0f).TrimBottom(LOWER_SIZE)
        )

        if (!improvements).Lamp <> Improvement.None then ScoreScreenHelpers.animation_queue.Add glint_animation

        base.Init parent

    override this.Draw() =
        Draw.rect (this.Bounds.Translate(10.0f, 10.0f)) Colors.black
        Background.draw (this.Bounds, (Color.FromArgb(40, 40, 40)), 2.0f)
        Draw.rect (this.Bounds.TrimBottom(LOWER_SIZE)) (score_info.Ruleset.LampColor (!lamp).Lamp).O1
        Draw.rect (this.Bounds.SliceBottom(LOWER_SIZE)) (score_info.Ruleset.LampColor (!lamp).Lamp).O2

        Glint.draw (float32 (glint_animation.Elapsed / glint_animation.Interval)) this.Bounds

        let text, color =
            match (!improvements).Lamp with
            | Improvement.New -> new_record, (Colors.text_yellow_2)
            | Improvement.Faster r -> sprintf "%s  •  +%gx" new_record (System.MathF.Round(r, 2)), (Colors.text_cyan_2)
            | Improvement.Better b ->
                let new_lamp = score_info.Ruleset.LampName (!lamp).Lamp
                let old_lamp = score_info.Ruleset.LampName((!lamp).Lamp - b)
                sprintf "%s  •  %s > %s" new_record old_lamp new_lamp, (Colors.text_green_2)
            | Improvement.FasterBetter(r, b) ->
                let new_lamp = score_info.Ruleset.LampName (!lamp).Lamp
                let old_lamp = score_info.Ruleset.LampName((!lamp).Lamp - b)

                sprintf "%s  •  %s > %s  •  +%gx" new_record old_lamp new_lamp (System.MathF.Round(r, 2)),
                (Colors.text_pink_2)
            | Improvement.None ->
                match (!previous_personal_bests) with
                | Some pbs ->
                    match PersonalBests.get_best_above_with_rate score_info.Rate pbs.Lamp with
                    | Some(v, r) ->

                        let summary =
                            if r > score_info.Rate then
                                sprintf "%s (%.2fx)" (score_info.Ruleset.LampName v) r
                            else
                                score_info.Ruleset.LampName v

                        [summary] %> "score.your_record", (Colors.grey_2.O2, Colors.black)

                    | None -> "--", (Colors.grey_2.O2, Colors.black)
                | None -> "--", (Colors.grey_2.O2, Colors.black)

        Text.fill_b (Style.font, text, this.Bounds.Shrink(10.0f, 0.0f).SliceBottom(LOWER_SIZE), color, Alignment.CENTER)
        base.Draw()

type Results(grade, lamp, improvements, previous_personal_bests, score_info) =
    inherit Container(NodeType.None)

    override this.Init(parent) =
        Container(
            NodeType.None,
            Position =
                { Position.Default with
                    Left = 0.35f %+ 0.0f
                }
        )
        |+ Grade(grade, score_info, Position = Position.Box(0.0f, 0.0f, 40.0f, 40.0f, 160.0f, 160.0f))
        |+ Accuracy(
            grade,
            improvements,
            previous_personal_bests,
            score_info,
            Position =
                {
                    Left = 0.0f %+ 200.0f ^+ 40.0f
                    Right = 0.5f %+ 100.0f ^- 20.0f
                    Top = 0.0f %+ 40.0f
                    Bottom = 0.0f %+ 200.0f
                }
        )
        |+ Lamp(
            lamp,
            improvements,
            previous_personal_bests,
            score_info,
            Position =
                {
                    Left = 0.5f %+ 100.0f ^+ 20.0f
                    Right = 1.0f %- 40.0f
                    Top = 0.0f %+ 40.0f
                    Bottom = 0.0f %+ 200.0f
                }
        )
        |> this.Add

        base.Init parent

    override this.Draw() =
        Draw.rect (this.Bounds.SliceTop(160.0f).TrimTop(5.0f)) Colors.shadow_2.O2
        Draw.rect (this.Bounds.TrimTop(160.0f).SliceTop(5.0f)) Colors.white
        base.Draw()