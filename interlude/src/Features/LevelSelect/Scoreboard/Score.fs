namespace Interlude.Features.LevelSelect

open System
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Calculator
open Prelude.Data.User
open Interlude.UI
open Interlude.Features.Score
open Interlude.Features.Gameplay

type private ScoreCard(score_info: ScoreInfo) =
    inherit
        Container(
            NodeType.Button(
                (fun () ->
                    Screen.change_new
                        (fun () -> new ScoreScreen(score_info, (ImprovementFlags.None, None), false) :> Screen)
                        ScreenType.Score
                        Transitions.EnterGameplayNoFadeAudio
                    |> ignore
                )
            )
        )

    let fade = Animation.Fade(0.0f, Target = 1.0f)
    let animation = Animation.seq [ Animation.Delay 200; fade ]
    let mod_string = score_info.ModString()

    override this.Init(parent) =
        this
        |* MouseListener()
            .Button(this)
            .OnRightClick(fun () -> ScoreContextMenu(false, score_info).Show())

        base.Init parent

    member this.Data = score_info

    member this.FadeOut() = fade.Target <- 0.0f

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Draw() =
        let bounds = this.Bounds

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

        // todo: dot the values that are your PBs

        Render.rect bounds fill_color
        Render.rect (bounds.BorderB Style.PADDING) border_color

        let mod_text =
            if score_info.Rate > SelectedChart.rate.Value then Icons.CHEVRONS_UP + "" + mod_string
            elif score_info.Rate < SelectedChart.rate.Value then Icons.CHEVRONS_DOWN + "" + mod_string
            else mod_string

        if this.Focused then

            Text.fill_b (
                Style.font,
                mod_text,
                bounds.SlicePercentT(0.6f).ShrinkL(450.0f).ShrinkR(10.0f),
                (Colors.white.O4a alpha, Colors.shadow_2.O4a alpha),
                Alignment.RIGHT
            )

            Text.fill_b (
                Style.font,
                if this.FocusedByMouse then %"scoreboard.view_hint_mouse" else [(%%"select").ToString()] %> "scoreboard.view_hint_keyboard"
                ,
                bounds.SlicePercentB(0.45f).ShrinkL(450.0f).ShrinkR(10.0f).TranslateY(-2.0f),
                (Colors.grey_1.O4a alpha, Colors.shadow_2.O4a alpha),
                Alignment.RIGHT
            )
        else

            Text.fill_b (
                Style.font,
                mod_text,
                bounds.ShrinkL(440.0f).Shrink(10.0f, 5.0f),
                (Colors.white.O4a alpha, Colors.shadow_2.O4a alpha),
                Alignment.RIGHT
            )

        Text.fill_b (
            Style.font,
            (if score_info.ImportedFromOsu then Icons.DOWNLOAD + " " else "") +
            format_timespan (DateTimeOffset.UtcNow - Timestamp.to_datetimeoffset score_info.TimePlayed)
            ,
            bounds.SliceL(80.0f).Shrink(10.0f, 5.0f),
            (Colors.grey_1.O4a alpha, Colors.shadow_2.O4a alpha),
            Alignment.CENTER
        )

        let box_color = Colors.shadow_2.O1a alpha

        let box = bounds.SliceL(140.0f).TranslateX(80.0f)
        Render.rect box box_color
        Text.fill_b (
            Style.font,
            score_info.Scoring.FormattedAccuracy,
            box.ShrinkY(5.0f),
            ((score_info.Ruleset.GradeColor score_info.Grade).O4a alpha, Colors.shadow_2.O4a alpha),
            Alignment.CENTER
        )

        let box = bounds.SliceL(100.0f).TranslateX(230.0f)
        Render.rect box box_color
        Text.fill_b (
            Style.font,
            score_info.Ruleset.LampName score_info.Lamp,
            box.ShrinkY(5.0f),
            ((score_info.Ruleset.LampColor score_info.Lamp).O4a alpha, Colors.shadow_2.O4a alpha),
            Alignment.CENTER
        )

        let box = bounds.SliceL(100.0f).TranslateX(340.0f)
        Render.rect box box_color
        Text.fill_b (
            Style.font,
            (sprintf "%.2f" score_info.Physical),
            box.ShrinkY(5.0f),
            (Colors.white.O4a alpha, (Difficulty.color score_info.Physical).O4a alpha),
            Alignment.CENTER
        )

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        animation.Update elapsed_ms

        if this.Focused && (not this.FocusedByMouse || Mouse.hover this.Bounds) then

            if (%%"delete").Pressed() then
                ScoreContextMenu.ConfirmDeleteScore(score_info, false)
            elif (%%"context_menu").Pressed() then
                ScoreContextMenu(false, score_info).Show()

        elif this.Focused && (%%"select").Pressed() then

            if this.FocusedByMouse then
                LevelSelect.choose_this_chart()
            else
                Screen.change_new
                        (fun () -> new ScoreScreen(score_info, (ImprovementFlags.None, None), false) :> Screen)
                        ScreenType.Score
                        Transitions.EnterGameplayNoFadeAudio
                    |> ignore