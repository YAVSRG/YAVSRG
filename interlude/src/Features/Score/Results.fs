namespace Interlude.Features.Score

open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude
open Prelude.Gameplay.Scoring
open Prelude.Gameplay
open Prelude.Data.User
open Interlude.UI

#nowarn "3370"

type Grade(grade: GradeResult ref, score_info: ScoreInfo) =
    inherit Container(NodeType.None)

    override this.Init(parent) =
        this
        |* Text((fun () -> score_info.Ruleset.GradeName (!grade).Grade))
            .Color((fun () -> (score_info.Ruleset.GradeColor (!grade).Grade, Colors.black)))
            .Position(Position.Expand(10.0f))

        base.Init parent

    override this.Draw() =
        Render.rect (this.Bounds.Translate(10.0f, 10.0f)) Colors.black
        Background.draw (this.Bounds, (Color.FromArgb(40, 40, 40)), 2.0f)
        let grade_color = score_info.Ruleset.GradeColor (!grade).Grade
        Render.rect this.Bounds grade_color.O1
        base.Draw()

type Accuracy
    (
        grade: GradeResult ref,
        improvements: ImprovementFlags ref,
        previous_personal_bests: Bests option ref,
        stats: ScoreScreenStats ref,
        score_info: ScoreInfo
    ) =
    inherit StaticWidget(NodeType.None)

    let LOWER_SIZE = 40.0f
    let new_record = sprintf "%s %s" Icons.AWARD (%"score.new_record")
    let mutable hover = false

    let glint_animation = Animation.Delay(375.0)

    override this.Init(parent) =
        if (!improvements).Accuracy <> Improvement.None then ScoreScreenHelpers.animation_queue.Add glint_animation
        base.Init parent

    override this.Update(elapsed_ms, moved) =
        hover <- Mouse.hover this.Bounds
        base.Update(elapsed_ms, moved)

    override this.Draw() =
        Render.rect (this.Bounds.Translate(10.0f, 10.0f)) Colors.black
        Background.draw (this.Bounds, (Color.FromArgb(40, 40, 40)), 2.0f)
        let grade_color = score_info.Ruleset.GradeColor (!grade).Grade
        Render.rect (this.Bounds.ShrinkB(LOWER_SIZE)) grade_color.O1
        Render.rect (this.Bounds.SliceB(LOWER_SIZE)) grade_color.O2

        Glint.draw_stencilled (float32 glint_animation.Progress) this.Bounds Glint.COLOR

        if (!stats).ColumnFilterApplied then
            Text.fill_b (
                Style.font,
                score_info.Scoring.Ruleset.FormatAccuracy (!stats).Accuracy,
                this.Bounds.Shrink(10.0f, 0.0f).ShrinkB(LOWER_SIZE),
                Colors.text_green,
                Alignment.CENTER
            )
        else
            Text.fill_b (
                Style.font,
                score_info.Scoring.FormattedAccuracy,
                this.Bounds.Shrink(10.0f, 0.0f).ShrinkB(LOWER_SIZE),
                (grade_color, Colors.black),
                Alignment.CENTER
            )

        let text, color =
            if (!stats).ColumnFilterApplied then
                let columns =
                    GraphSettings.column_filter
                    |> Seq.indexed
                    |> Seq.choose (fun (i, b) -> if b && i < score_info.WithMods.Keys then Some ((i + 1).ToString()) else None)
                    |> String.concat " "
                sprintf "%s: %s" %"score.graph.settings.column_filter" columns, Colors.text_green
            else

            match (!improvements).Accuracy with
            | Improvement.New -> new_record, Colors.text_yellow_2
            | Improvement.Faster r -> sprintf "%s  •  +%gx" new_record (System.MathF.Round(float32 r, 2)), Colors.text_cyan_2
            | Improvement.Better b -> sprintf "%s  •  +%.2f%%" new_record (b * 100.0), Colors.text_green_2
            | Improvement.FasterBetter(r, b) ->
                sprintf "%s  •  +%.2f%%  •  +%gx" new_record (b * 100.0) (System.MathF.Round(float32 r, 2)), Colors.text_pink_2
            | Improvement.None ->
                match (!previous_personal_bests) with
                | Some pbs ->
                    match PersonalBests.get_best_above score_info.Rate pbs.Accuracy with
                    | Some(v, r, _) ->

                        let summary, distance_from_pb =
                            if r > score_info.Rate then
                                sprintf "%s (%.2fx)" (score_info.Ruleset.FormatAccuracy v) r, (v - score_info.Scoring.Accuracy)
                            else
                                score_info.Ruleset.FormatAccuracy v, (v - score_info.Scoring.Accuracy)

                        if distance_from_pb < 0.0001 then
                            [summary] %> "score.your_record", (Colors.grey_2.O2, Colors.black)
                        else
                            [sprintf "%.2f%%" (distance_from_pb * 100.0); summary] %> "score.compare_accuracy",
                            (Colors.grey_2.O2, Colors.black)

                    | None -> "--", (Colors.grey_2.O2, Colors.black)
                | None -> "--", (Colors.grey_2.O2, Colors.black)

        Text.fill_b (Style.font, text, this.Bounds.Shrink(10.0f, 0.0f).SliceB(LOWER_SIZE), color, Alignment.CENTER)

        if hover then
            let acc_tooltip = this.Bounds.SliceX(150.0f).BorderB(60.0f).TranslateY(15.0f)
            Render.rect (acc_tooltip.Expand(Style.PADDING)) Colors.white
            Render.rect acc_tooltip Colors.shadow_2

            Text.fill_b (
                Style.font,
                sprintf "%.4f%%" (stats.Value.Accuracy * 100.0),
                acc_tooltip.Shrink(10.0f, 5.0f),
                (if stats.Value.ColumnFilterApplied then Colors.text_green else Colors.text),
                Alignment.CENTER
            )

type Lamp
    (
        lamp: LampResult ref,
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
        |* Text((fun () -> score_info.Ruleset.LampName (!lamp).Lamp))
            .Color((fun () -> (score_info.Ruleset.LampColor (!lamp).Lamp, Colors.black)))
            .Position(Position.ShrinkX(10.0f).ShrinkB(LOWER_SIZE))

        if (!improvements).Lamp <> Improvement.None then ScoreScreenHelpers.animation_queue.Add glint_animation

        base.Init parent

    override this.Update(elapsed_ms, moved) =
        hover <- Mouse.hover this.Bounds
        base.Update(elapsed_ms, moved)

    override this.Draw() =
        Render.rect (this.Bounds.Translate(10.0f, 10.0f)) Colors.black
        Background.draw (this.Bounds, (Color.FromArgb(40, 40, 40)), 2.0f)
        Render.rect (this.Bounds.ShrinkB(LOWER_SIZE)) (score_info.Ruleset.LampColor (!lamp).Lamp).O1
        Render.rect (this.Bounds.SliceB(LOWER_SIZE)) (score_info.Ruleset.LampColor (!lamp).Lamp).O2

        Glint.draw_stencilled (float32 glint_animation.Progress) this.Bounds Glint.COLOR

        let text, color =
            match (!improvements).Lamp with
            | Improvement.New -> new_record, (Colors.text_yellow_2)
            | Improvement.Faster r -> sprintf "%s  •  +%gx" new_record (System.MathF.Round(float32 r, 2)), (Colors.text_cyan_2)
            | Improvement.Better b ->
                let new_lamp = score_info.Ruleset.LampName (!lamp).Lamp
                let old_lamp = score_info.Ruleset.LampName((!lamp).Lamp - b)
                sprintf "%s  •  %s > %s" new_record old_lamp new_lamp, (Colors.text_green_2)
            | Improvement.FasterBetter(r, b) ->
                let new_lamp = score_info.Ruleset.LampName (!lamp).Lamp
                let old_lamp = score_info.Ruleset.LampName((!lamp).Lamp - b)

                sprintf "%s  •  %s > %s  •  +%gx" new_record old_lamp new_lamp (System.MathF.Round(float32 r, 2)),
                (Colors.text_pink_2)
            | Improvement.None ->
                match (!previous_personal_bests) with
                | Some pbs ->
                    match PersonalBests.get_best_above score_info.Rate pbs.Lamp with
                    | Some(v, r, _) ->

                        let summary =
                            if r > score_info.Rate then
                                sprintf "%s (%.2fx)" (score_info.Ruleset.LampName v) r
                            else
                                score_info.Ruleset.LampName v

                        [summary] %> "score.your_record", (Colors.grey_2.O2, Colors.black)

                    | None -> "--", (Colors.grey_2.O2, Colors.black)
                | None -> "--", (Colors.grey_2.O2, Colors.black)

        Text.fill_b (Style.font, text, this.Bounds.Shrink(10.0f, 0.0f).SliceB(LOWER_SIZE), color, Alignment.CENTER)
        base.Draw()

        if hover then
            let raw_greats_tooltip = this.Bounds.SliceX(150.0f).BorderB(60.0f).TranslateY(15.0f)
            Render.rect (raw_greats_tooltip.Expand(Style.PADDING)) Colors.white
            Render.rect raw_greats_tooltip Colors.shadow_2

            Text.fill_b (
                Style.font,
                sprintf "%.1f raw" (score_info.Scoring.MaxPossiblePoints - score_info.Scoring.PointsScored),
                raw_greats_tooltip.Shrink(10.0f, 5.0f),
                Colors.text,
                Alignment.CENTER
            )

type Results
    (
        grade: GradeResult ref,
        lamp: LampResult ref,
        improvements: ImprovementFlags ref,
        previous_personal_bests: Bests option ref,
        stats: ScoreScreenStats ref, score_info: ScoreInfo
    ) =
    inherit Container(NodeType.None)

    override this.Init(parent) =
        this
        |+ Grade(grade, score_info)
            .Position(Position.Box(0.0f, 0.0f, 40.0f, 40.0f, 160.0f, 160.0f))
        |+ Accuracy(
            grade,
            improvements,
            previous_personal_bests,
            stats,
            score_info
        )
            .Position(
                {
                    Left = 0.0f %+ 200.0f ^+ 40.0f
                    Right = 0.5f %+ 100.0f ^- 20.0f
                    Top = 0.0f %+ 40.0f
                    Bottom = 0.0f %+ 200.0f
                }
            )
        |* Lamp(
            lamp,
            improvements,
            previous_personal_bests,
            score_info
        )
            .Position(
                {
                    Left = 0.5f %+ 100.0f ^+ 20.0f
                    Right = 1.0f %- 40.0f
                    Top = 0.0f %+ 40.0f
                    Bottom = 0.0f %+ 200.0f
                }
            )

        base.Init parent