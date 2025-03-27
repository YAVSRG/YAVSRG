namespace Interlude.Features.Score

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.User
open Prelude.Data.User.Stats
open Interlude.UI

type TopBanner(score_info: ScoreInfo) as this =
    inherit Container(NodeType.None)

    do
        this
        |+ Text(score_info.ChartMeta.Artist + " - " + score_info.ChartMeta.Title)
            .Align(Alignment.LEFT)
            .Position(Position.SliceT(85.0f).ShrinkX(20.0f).ShrinkR(300.0f))
        |+ Text(score_info.ChartMeta.DifficultyName)
            .Align(Alignment.LEFT)
            .Position(Position.SliceT(75.0f, 55.0f).ShrinkX(20.0f))
        |+ Text(sprintf "%s  •  %s" ([score_info.ChartMeta.OriginString] %> "score.source") ([score_info.ChartMeta.Creator] %> "score.creator"))
            .Align(Alignment.LEFT)
            .Position(Position.SliceT(125.0f, 40.0f).ShrinkX(20.0f))

        |+ Text((score_info.TimePlayed |> Timestamp.to_datetime).ToLocalTime().ToString())
            .Align(Alignment.RIGHT)
            .Position(Position.SliceT(65.0f, 55.0f).ShrinkX(20.0f))
        |* Text(
            match score_info.PlayedBy with
            | ScorePlayedBy.Username p -> K([p] %> "score.played_by")
            | ScorePlayedBy.You -> (fun () -> [format_short_time CURRENT_SESSION.GameTime; format_short_time CURRENT_SESSION.PlayTime] %> "score.session_time")
            )
            .Align(Alignment.RIGHT)
            .Position(Position.SliceT(115.0f, 50.0f).ShrinkX(20.0f))

        match score_info.PlayedBy with
        | ScorePlayedBy.You ->
            this.Add(
                Username()
                    .Position(Position.SliceT(Toolbar.HEIGHT).SliceR(300.0f))
            )
        | _ -> ()

    override this.Draw() =

        Render.rect (this.Bounds.ShrinkB 5.0f) (Palette.color (127, 0.5f, 0.0f))
        Render.rect (this.Bounds.SliceB 5.0f) Colors.white.O2

        base.Draw()