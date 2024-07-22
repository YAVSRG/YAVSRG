namespace Interlude.Features.Online.Players

open System
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
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
            + " ago"
        )

    override this.Draw() =
        Draw.rect this.Bounds Colors.shadow_2.O2
        let h = this.Bounds.Height / 10.0f
        let mutable y = 0.0f

        for score, ago in scores do

            let b = this.Bounds.TrimTop(y).SliceTop(h)

            Text.fill_b (Style.font, score.Title, b.SliceTop(50.0f).Shrink(10.0f, 0.0f), Colors.text, Alignment.LEFT)

            Text.fill_b (
                Style.font,
                score.Artist + "  •  " + score.Difficulty + "  •  " + ago,
                b.TrimTop(50.0f).Shrink(10.0f, 0.0f).Translate(0.0f, -5.0f),
                Colors.text_subheading,
                Alignment.LEFT
            )

            Text.fill_b (
                Style.font,
                format_accuracy score.Score,
                b.TrimRight(h * 3.5f).SliceRight(h * 2.0f).Shrink(0.0f, h * 0.2f),
                Colors.text,
                Alignment.RIGHT
            )

            Text.fill_b (
                Style.font,
                score.Lamp,
                b.TrimRight(h * 2.0f).SliceRight(h).Shrink(0.0f, h * 0.2f),
                Colors.text,
                Alignment.CENTER
            )

            Text.fill_b (
                Style.font,
                score.Mods,
                b.TrimRight(h * 0.5f).SliceRight(h).Shrink(0.0f, h * 0.2f),
                Colors.text,
                Alignment.CENTER
            )

            y <- y + h