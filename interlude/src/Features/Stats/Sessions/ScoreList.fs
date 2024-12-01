﻿namespace Interlude.Features.Stats

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Prelude
open Prelude.Data.User
open Prelude.Data.Library
open Prelude.Gameplay.Rulesets
open Interlude.Content

type MissingScore() =
    inherit StaticWidget(NodeType.None)

    let fade = Animation.Fade(0.0f, Target = 1.0f)

    override this.Draw() =
        let alpha = fade.Alpha

        let fill_color =
            if this.Focused then
                Colors.yellow_accent.O1a alpha
            else
                (!*Palette.DARK).O1a alpha

        let border_color =
            if this.Focused then
                Colors.yellow_accent.O2a alpha
            else
                (!*Palette.LIGHT).O2a alpha

        Draw.rect this.Bounds fill_color
        Draw.rect (this.Bounds.BorderL Style.PADDING) border_color

        Text.fill_b(Style.font, "<Missing the chart for this score>", this.Bounds.SliceY(50.0f), (Colors.grey_2.O4a alpha, Colors.shadow_2.O4a alpha), Alignment.CENTER)

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        fade.Update elapsed_ms

// todo: clicking loads the song + opens score screen
type Score(score_info: ScoreInfo) =
    inherit Container(NodeType.None)

    let mod_string = score_info.ModString()

    let fade = Animation.Fade(0.0f, Target = 1.0f)

    override this.Draw() =
        let alpha = fade.Alpha

        let fill_color =
            if this.Focused then
                Colors.yellow_accent.O1a alpha
            else
                (!*Palette.DARK).O1a alpha

        let border_color =
            if this.Focused then
                Colors.yellow_accent.O2a alpha
            else
                (!*Palette.LIGHT).O2a alpha

        Draw.rect this.Bounds fill_color
        Draw.rect (this.Bounds.BorderL Style.PADDING) border_color

        Text.fill_b (
            Style.font,
            score_info.ChartMeta.Title,
            this.Bounds.SliceT(35.0f).ShrinkR(420.0f).ShrinkL(10.0f),
            (Colors.white.O4a alpha, Colors.shadow_2.O4a alpha),
            Alignment.LEFT
        )

        Text.fill_b (
            Style.font,
            score_info.ChartMeta.Artist + "  •  " + score_info.ChartMeta.Creator,
            this.Bounds.ShrinkT(30.0f).SliceT(30.0f).ShrinkR(420.0f).ShrinkL(10.0f).TranslateY(-2.0f),
            (Colors.grey_1.O4a alpha, Colors.shadow_2.O4a alpha),
            Alignment.LEFT
        )

        Text.fill_b (
            Style.font,
            score_info.ChartMeta.DifficultyName,
            this.Bounds.SliceB(25.0f).ShrinkR(420.0f).ShrinkL(10.0f),
            (Colors.grey_1.O4a alpha, Colors.shadow_2.O4a alpha),
            Alignment.LEFT
        )


        let box_color = Colors.shadow_2.O1a alpha
        let box = this.Bounds.SliceR(360.0f).ShrinkT(50.0f).TranslateX(-10.0f)
        Draw.rect box box_color
        Text.fill_b (
            Style.font,
            if this.Focused then
                if this.FocusedByMouse then %"scoreboard.view_hint_mouse" else [(%%"select").ToString()] %> "scoreboard.view_hint_keyboard"
            else 
                mod_string
            ,
            this.Bounds.ShrinkT(50.0f).SliceR(370.0f).ShrinkX(10.0f),
            (Colors.grey_1.O4a alpha, Colors.shadow_2.O4a alpha),
            Alignment.CENTER
        )
        
        let box = this.Bounds.SliceR(140.0f).SliceT(45.0f).TranslateX(-230.0f)
        Draw.rect box box_color
        Text.fill_b (
            Style.font,
            score_info.Scoring.FormattedAccuracy,
            box.ShrinkY(5.0f),
            ((score_info.Ruleset.GradeColor score_info.Grade).O4a alpha, Colors.shadow_2.O4a alpha),
            Alignment.CENTER
        )
        
        let box = this.Bounds.SliceR(100.0f).SliceT(45.0f).TranslateX(-120.0f)
        Draw.rect box box_color
        Text.fill_b (
            Style.font,
            score_info.Ruleset.LampName score_info.Lamp,
            box.ShrinkY(5.0f),
            ((score_info.Ruleset.LampColor score_info.Lamp).O4a alpha, Colors.shadow_2.O4a alpha),
            Alignment.CENTER
        )

        let box = this.Bounds.SliceR(100.0f).SliceT(45.0f).TranslateX(-10.0f)
        Draw.rect box box_color
        Text.fill_b (
            Style.font,
            (sprintf "%.2f" score_info.Physical),
            box.ShrinkY(5.0f),
            (Colors.white.O4a alpha, (Charts.Processing.Difficulty.DifficultyRating.physical_color score_info.Physical).O4a alpha),
            Alignment.CENTER
        )

    override this.Init(parent: Widget) =
        this |* Clickable.Focus this

        base.Init parent

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        fade.Update elapsed_ms

module private ScoreList =

    let loader =
        { new Async.SwitchServiceSeq<int64 * int64 * Ruleset * (Widget -> unit), unit -> unit>() with
            member this.Process((start_time, end_time, ruleset, callback)) =
                seq {
                    for chart_hash, score in UserDatabase.get_scores_between start_time end_time Content.UserData do
                        match ChartDatabase.get_meta_cached chart_hash Content.Charts with
                        | Some cc ->
                            match ChartDatabase.get_chart chart_hash Content.Charts with
                            | Ok chart ->
                                let score_info = ScoreInfo.from_score cc chart ruleset score
                                yield fun () -> callback(Score(score_info))
                            | _ -> ()
                        | _ -> yield fun () -> callback(MissingScore())
                }

            member this.Handle(action) = action ()
        }

type ScoreList(start_time: int64, end_time: int64) =
    inherit Container(NodeType.None)

    let scores = FlowContainer.Vertical<Widget>(80.0f, Spacing = 5.0f)

    override this.Init(parent: Widget) =
        this
        |* ScrollContainer(scores, Position = Position.Shrink(20.0f), Margin = 5.0f)

        ScoreList.loader.Request((start_time, end_time, Rulesets.current, scores.Add))

        base.Init parent
        
        // todo: export as playlist

    override this.Draw() =
        Draw.rect this.Bounds Colors.shadow_2.O2
        base.Draw()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        ScoreList.loader.Join()