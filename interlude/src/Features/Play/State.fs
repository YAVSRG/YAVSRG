namespace Interlude.Features.Play

open Interlude.Content
open Prelude.Scoring

[<RequireQualifiedAccess>]
type PacemakerInfo =
    | None
    | Accuracy of float
    | Replay of IScoreMetric
    | Judgement of target: JudgementId * max_count: int

type PlayState =
    {
        Ruleset: Ruleset
        Scoring: IScoreMetric
        CurrentChartTime: unit -> ChartTime
        Pacemaker: PacemakerInfo
    }
    static member Dummy(chart) =
        let s = Metrics.createDummyMetric chart
        {
            Ruleset = Unchecked.defaultof<_>
            Scoring = s
            CurrentChartTime = Unchecked.defaultof<_>
            Pacemaker = PacemakerInfo.None
        }