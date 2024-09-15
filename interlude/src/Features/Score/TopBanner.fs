namespace Interlude.Features.Score

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.User
open Interlude.Features.Stats

type TopBanner(score_info: ScoreInfo) as this =
    inherit Container(NodeType.None)

    do
        this
        |+ Text(
            score_info.ChartMeta.Artist + " - " + score_info.ChartMeta.Title,
            Align = Alignment.LEFT,
            Position =
                {
                    Left = 0.0f %+ 20.0f
                    Top = 0.0f %+ 0.0f
                    Right = 1.0f %- 20.0f
                    Bottom = 0.0f %+ 85.0f
                }
        )
        |+ Text(
            score_info.ChartMeta.DifficultyName,
            Align = Alignment.LEFT,
            Position =
                {
                    Left = 0.0f %+ 20.0f
                    Top = 0.0f %+ 75.0f
                    Right = 1.0f %- 20.0f
                    Bottom = 0.0f %+ 130.0f
                }
        )
        |+ Text(
            sprintf "%s  •  %s" ([score_info.ChartMeta.OriginString] %> "score.source") ([score_info.ChartMeta.Creator] %> "score.creator"),
            Align = Alignment.LEFT,
            Position =
                {
                    Left = 0.0f %+ 20.0f
                    Top = 0.0f %+ 125.0f
                    Right = 1.0f %+ 0.0f
                    Bottom = 0.0f %+ 165.0f
                }
        )

        |+ Text(
            (score_info.TimePlayed |> Timestamp.to_datetime).ToLocalTime().ToString(),
            Align = Alignment.RIGHT,
            Position =
                {
                    Left = 0.0f %+ 0.0f
                    Top = 0.0f %+ 75.0f
                    Right = 1.0f %- 20.0f
                    Bottom = 0.0f %+ 130.0f
                }
        )
        |* Text(
            match score_info.PlayedBy with
            | ScorePlayedBy.Username p -> K([p] %> "score.played_by")
            | ScorePlayedBy.You -> (fun () -> [Stats.format_short_time Stats.session.GameTime; Stats.format_short_time Stats.session.PlayTime] %> "score.session_time")
            , Align = Alignment.RIGHT
            , Position =
                {
                    Left = 0.0f %+ 0.0f
                    Top = 0.0f %+ 125.0f
                    Right = 1.0f %- 20.0f
                    Bottom = 0.0f %+ 165.0f
                }
        )

    override this.Draw() =

        Draw.rect (this.Bounds.ShrinkB 5.0f) (Palette.color (127, 0.5f, 0.0f))
        Draw.rect (this.Bounds.SliceB 5.0f) Colors.white.O2

        base.Draw()