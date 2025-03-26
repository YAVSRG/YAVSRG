namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.User
open Interlude.UI
open Interlude.Features.Score
open Interlude.Web.Shared.Requests

type LeaderboardScore = Charts.Scores.Leaderboard.Score

type private LeaderboardCard(score: LeaderboardScore, score_info: ScoreInfo) =
    inherit
        FrameContainer(
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
    let animation = Animation.seq [ Animation.Delay 150; fade ]

    override this.Init(parent) =
        this.Fill <-
            fun () ->
                if this.Focused then
                    Colors.yellow_accent.O1a fade.Alpha
                else
                    (!*Palette.DARK).O2a fade.Alpha

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

        this
        |+ Text(fun () ->
            sprintf "#%i %s  •  %s"
                score.Rank
                score.Username
                score_info.Scoring.FormattedAccuracy
        )
            .Color(text_color)
            .Align(Alignment.LEFT)
            .Position(Position.SlicePercentT(0.6f).SlicePercentL(0.6f).ShrinkL(10.0f))

        |+ Text(fun () ->
            sprintf
                "%s  •  %ix  •  %.2f"
                (score_info.Ruleset.LampName score_info.Lamp)
                score_info.Scoring.BestCombo
                score_info.Physical
        )
            .Color(text_subcolor)
            .Align(Alignment.LEFT)
            .Position(Position.SlicePercentB(0.4f).SlicePercentL(0.5f).ShrinkL(10.0f).ExpandT(5.0f).ShrinkB(2.0f))

        |+ Text(format_timespan (Timestamp.since score_info.TimePlayed))
            .Color(text_subcolor)
            .Align(Alignment.RIGHT)
            .Position(Position.SlicePercentB(0.4f).SlicePercentR(0.5f).ShrinkR(10.0f).ExpandT(5.0f).ShrinkB(2.0f))

        |+ Text(score_info.ModString())
            .Color(text_color)
            .Align(Alignment.RIGHT)
            .Position(Position.SlicePercentT(0.6f).SlicePercentR(0.4f).ShrinkR(10.0f))

        |* MouseListener()
            .Button(this)
            .OnRightClick(fun () -> ScoreContextMenu(true, score_info).Show())

        base.Init parent

    member this.Data = score_info

    member this.FadeOut() = fade.Target <- 0.0f

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        animation.Update elapsed_ms

        if this.Focused && (not this.FocusedByMouse || Mouse.hover this.Bounds) then

            if (%%"context_menu").Pressed() then
                ScoreContextMenu(true, score_info).Show()

        elif this.Focused && (%%"select").Pressed() then

            if this.FocusedByMouse then
                LevelSelect.choose_this_chart()
            else
                Screen.change_new
                        (fun () -> new ScoreScreen(score_info, (ImprovementFlags.None, None), false) :> Screen)
                        ScreenType.Score
                        Transitions.EnterGameplayNoFadeAudio
                    |> ignore