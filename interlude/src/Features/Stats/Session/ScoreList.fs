namespace Interlude.Features.Stats

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude.Data.User
open Prelude.Data.Library
open Interlude.Content

type Score(score_info: ScoreInfo) =
    inherit FrameContainer(NodeType.None, Fill = K Colors.shadow_2.O2, Border = K Colors.white.O2)

    override this.Init(parent: Widget) =
        this
        |+ Text(sprintf "%s - %s" score_info.ChartMeta.Artist score_info.ChartMeta.Title, Position = Position.ShrinkT(40.0f))
        |* Text(sprintf "%.2f%%, %s" (score_info.Accuracy * 100.0) (score_info.Ruleset.LampName score_info.Lamp), Position = Position.SliceT(40.0f))

        base.Init parent

type ScoreList(start_time: int64, end_time: int64) =
    inherit Container(NodeType.None)

    let scores = FlowContainer.Vertical<Score>(60.0f, Spacing = 15.0f)

    override this.Init(parent: Widget) =
        this
        |* ScrollContainer(scores, Position = Position.Shrink(20.0f), Margin = 5.0f)

        for chart_hash, score in UserDatabase.get_scores_between start_time end_time Content.UserData do
            match ChartDatabase.get_meta_cached chart_hash Content.Charts with
            | Some cc ->
                match ChartDatabase.get_chart chart_hash Content.Charts with
                | Ok chart ->
                    let score_info = ScoreInfo.from_score cc chart Rulesets.current score
                    scores.Add(Score(score_info))
                | _ -> ()
            | _ -> () // add "missing chart" placeholder

        base.Init parent
        
        // todo: export as playlist

    override this.Draw() =
        Draw.rect this.Bounds Colors.shadow_2.O2
        base.Draw()