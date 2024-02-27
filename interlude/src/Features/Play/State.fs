namespace Interlude.Features.Play

open System
open Prelude
open Prelude.Charts
open Prelude.Charts.Tools
open Prelude.Charts.Tools.NoteColors
open Prelude.Gameplay
open Interlude.Features.Gameplay.Chart

[<RequireQualifiedAccess>]
type PacemakerInfo =
    | None
    | Accuracy of float
    | Replay of IScoreMetric
    | Judgement of target: JudgementId * max_count: int

type PlayState =
    {
        Chart: Chart
        WithColors: ColoredChart
        Ruleset: Ruleset
        mutable Scoring: IScoreMetric
        ScoringChanged: Event<unit>
        CurrentChartTime: unit -> ChartTime
        Pacemaker: PacemakerInfo
    }
    static member Dummy (info: LoadedChartInfo) =
        let s = Metrics.create_dummy info.WithMods

        {
            Chart = info.Chart
            WithColors = info.WithMods
            Ruleset = s.Ruleset
            Scoring = s
            ScoringChanged = Event<unit>()
            CurrentChartTime = fun () -> 0.0f<ms>
            Pacemaker = PacemakerInfo.None
        }

    member this.SubscribeToHits(handler: HitEvent<HitEventGuts> -> unit) =
        let mutable obj: IDisposable = this.Scoring.OnHit.Subscribe handler

        this.ScoringChanged.Publish.Add(fun () ->
            obj.Dispose()
            obj <- this.Scoring.OnHit.Subscribe handler
        )

    member this.ChangeScoring(scoring) =
        this.Scoring <- scoring
        this.ScoringChanged.Trigger()
