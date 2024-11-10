namespace Interlude.Features.LevelSelect

open System
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay
open Prelude.Data.User
open Interlude.UI
open Interlude.Features.Score
open Interlude.Features.Gameplay

type private ScoreCard(score_info: ScoreInfo) =
    inherit
        FrameContainer(
            NodeType.Button(
                (fun () ->
                    Screen.change_new
                        (fun () -> new ScoreScreen(score_info, ImprovementFlags.None, false) :> Screen)
                        Screen.Type.Score
                        Transitions.EnterGameplayNoFadeAudio
                    |> ignore
                )
            )
        )

    let fade = Animation.Fade(0.0f, Target = 1.0f)
    let animation = Animation.seq [ Animation.Delay 150; fade ]

    override this.Init(parent) =
        this.Fill <-
            fun () ->
                if this.Focused then
                    Colors.yellow_accent.O1a fade.Alpha
                else
                    (!*Palette.DARK).O1a fade.Alpha

        this.Border <-
            fun () ->
                if this.Focused then
                    Colors.yellow_accent.O4a fade.Alpha
                else
                    (!*Palette.LIGHT).O2a fade.Alpha

        let text_color =
            fun () -> let a = fade.Alpha in (Colors.white.O4a a, Colors.shadow_1.O4a a)

        let text_subcolor =
            fun () -> let a = fade.Alpha in (Colors.grey_1.O4a a, Colors.shadow_2.O4a a)

        let upper = Position.SliceT(47.5f).Shrink(10.0f, 1f).TranslateY(-2.0f)
        let lower = Position.ShrinkT(37.5f).Shrink(10.0f, 1f).TranslateY(-1.0f)

        this
        |+ Text(
            fun () -> score_info.Scoring.FormattedAccuracy
            , Color = (fun () -> let a = fade.Alpha in (score_info.Ruleset.GradeColor score_info.Grade).O4a a, Colors.shadow_2.O4a a)
            , Align = Alignment.LEFT
            , Position = upper
        )

        |+ Text(
            fun () ->
                sprintf
                    "%s  •  %ix  •  %.2f"
                    (score_info.Ruleset.LampName score_info.Lamp)
                    score_info.Scoring.BestCombo
                    score_info.Physical
            , Color = text_subcolor
            , Align = Alignment.LEFT
            , Position = lower
        )

        |+ Text(
            (
                let normal_text = 
                    (if score_info.ImportedFromOsu then Icons.DOWNLOAD + " " else "") +
                    format_timespan (DateTimeOffset.UtcNow - Timestamp.to_datetimeoffset score_info.TimePlayed)
                fun () ->
                    if this.Focused then
                        if this.FocusedByMouse then %"scoreboard.view_hint_mouse" else [(%%"select").ToString()] %> "scoreboard.view_hint_keyboard"
                    else normal_text
            ),
            Color = text_subcolor,
            Align = Alignment.RIGHT,
            Position = lower
        )

        |+ Text(
            (let ms = score_info.ModString() in fun () -> 
                if score_info.Rate > SelectedChart.rate.Value then Icons.CHEVRONS_UP + "" + ms
                elif score_info.Rate < SelectedChart.rate.Value then Icons.CHEVRONS_DOWN + "" + ms
                else ms
            ),
            Color = text_color,
            Align = Alignment.RIGHT,
            Position = upper
        )

        |* Clickable.Focus(this, OnRightClick = (fun () -> ScoreContextMenu(score_info).Show()))

        base.Init parent

    member this.Data = score_info

    member this.FadeOut() = fade.Target <- 0.0f

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        animation.Update elapsed_ms

        if Mouse.hover this.Bounds && (%%"delete").Tapped() then
            ScoreContextMenu.ConfirmDeleteScore(score_info, false)
        elif this.Focused && not this.FocusedByMouse && (%%"context_menu").Tapped() then
            ScoreContextMenu(score_info).Show()
        elif this.Focused && (%%"select").Tapped() then
            if this.FocusedByMouse then 
                LevelSelect.choose_this_chart() 
            else 
                Screen.change_new
                        (fun () -> new ScoreScreen(score_info, ImprovementFlags.None, false) :> Screen)
                        Screen.Type.Score
                        Transitions.EnterGameplayNoFadeAudio
                    |> ignore