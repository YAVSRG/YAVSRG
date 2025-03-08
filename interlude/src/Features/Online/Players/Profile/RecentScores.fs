namespace Interlude.Features.Online.Players

open System
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Gameplay.Rulesets
open Interlude.Web.Shared.Requests
open Interlude.Features.Online

type private RecentScores(scores: Players.Profile.View.RecentScore array) =
    inherit StaticWidget(NodeType.None)

    let scores =
        scores
        |> Array.map (fun score ->
            score,
            (DateTimeOffset.UtcNow - DateTimeOffset.FromUnixTimeMilliseconds(score.Timestamp)
             |> format_timespan)
            + " ago",
            Gameplay.Scoring.Grade.calculate SC_J4.Grades score.Score |> SC_J4.GradeColor,
            SC_J4.LampColor score.Lamp
        )

    override this.Draw() =
        Render.rect this.Bounds Colors.shadow_2.O2
        let h = this.Bounds.Height / 10.0f
        let mutable y = 0.0f

        for score, ago, grade_color, lamp_color in scores do

            let b = this.Bounds.ShrinkT(y).SliceT(h)

            Text.fill_b (Style.font, score.Title, b.SliceT(45.0f).ShrinkX(10.0f), Colors.text, Alignment.LEFT)

            Text.fill_b (
                Style.font,
                score.Artist + "  •  " + score.Difficulty + "  •  " + ago,
                b.ShrinkT(45.0f).ShrinkX(10.0f).TranslateY(-5.0f),
                Colors.text_subheading,
                Alignment.LEFT
            )

            Text.fill_b (
                Style.font,
                sprintf "%.2f%%" (score.Score * 100.0),
                b.ShrinkR(h * 3.5f).SliceR(h * 2.0f).ShrinkY(h * 0.2f),
                (grade_color, Colors.shadow_2),
                Alignment.RIGHT
            )

            Text.fill_b (
                Style.font,
                SC_J4.LampName score.Lamp,
                b.ShrinkR(h * 2.0f).SliceR(h).ShrinkY(h * 0.2f),
                (lamp_color, Colors.shadow_2),
                Alignment.CENTER
            )

            Text.fill_b (
                Style.font,
                (if score.Mods.IsEmpty then sprintf "%.2fx" score.Rate else sprintf"%.2fx*" score.Rate),
                b.ShrinkR(h * 0.5f).SliceR(h).ShrinkY(h * 0.2f),
                Colors.text,
                Alignment.CENTER
            )

            y <- y + h