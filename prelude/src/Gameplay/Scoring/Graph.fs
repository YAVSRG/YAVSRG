namespace Prelude.Gameplay.Scoring

open Prelude
open Prelude.Gameplay.Replays
open Prelude.Gameplay.Rulesets

type GraphPoint =
    {
        Time: ChartTime
        PointsScored: float
        MaxPointsScored: float
        Combo: int
        Lamp: int
        Mean: GameplayTime
        StandardDeviation: GameplayTime
        Judgements: int array
    }
    member this.Accuracy = if this.MaxPointsScored = 0.0 then 1.0 else this.PointsScored / this.MaxPointsScored

type ScoreScreenStats =
    {
        Notes: int * int
        Holds: int * int
        Releases: int * int
        GhostTaps: int

        TapMean: GameplayTime
        TapStandardDeviation: GameplayTime
        TapEarlyPercent: float
        TapRange: GameplayTime * GameplayTime

        ReleaseMean: GameplayTime
        ReleaseStandardDeviation: GameplayTime
        ReleaseEarlyPercent: float
        ReleaseRange: GameplayTime * GameplayTime

        Judgements: int array
        JudgementCount: int
        MA: string
        PA: string

        Accuracy: float

        GraphPoints: GraphPoint array

        ColumnFilterApplied: bool
    }

module ScoreScreenStats =

    let GRAPH_POINT_COUNT = 1000

    let calculate (score_processor: ScoreProcessor) (column_filter: bool array) : ScoreScreenStats =

        let inc (x: int ref) = x.Value <- x.Value + 1
        let (++) (x: GameplayTime ref) (t: GameplayTime) = x.Value <- x.Value + t

        let filtered_judgements = Array.zeroCreate score_processor.Ruleset.Judgements.Length
        let graph_points = ResizeArray<GraphPoint>()

        let taps = ref 0
        let early_taps = ref 0
        let tap_sum = ref 0.0f<ms / rate>
        let tap_sumOfSq = ref 0.0f<ms / rate> // f# type system bug
        let earliest_tap = ref 0.0f<ms / rate>
        let latest_tap = ref 0.0f<ms / rate>

        let releases = ref 0
        let early_releases = ref 0
        let release_sum = ref 0.0f<ms / rate>
        let release_sumOfSq = ref 0.0f<ms / rate>
        let earliest_release = ref 0.0f<ms / rate>
        let latest_release = ref 0.0f<ms / rate>

        let notes_hit = ref 0
        let notes_count = ref 0
        let holds_held = ref 0
        let holds_count = ref 0
        let releases_released = ref 0
        let releases_count = ref 0

        let ghost_taps = ref 0

        let mutable combo = 0
        let mutable combo_breaks = 0
        let mutable max_points = 0.0
        let mutable scored_points = 0.0

        let add_action(ev: GameplayEvent) =
            match ev.Action with
            | Hit e ->
                if not e.Missed then
                    inc notes_hit

                inc notes_count

                if not e.Missed then
                    inc taps
                    tap_sum ++ e.Delta
                    tap_sumOfSq ++ e.Delta * float32 e.Delta
                    earliest_tap.Value <- min earliest_tap.Value e.Delta
                    latest_tap.Value <- max latest_tap.Value e.Delta
                    if e.Delta < 0.0f<ms / rate> then
                        inc early_taps

                match e.Judgement with
                | Some (j, points) ->
                    filtered_judgements.[j] <- filtered_judgements.[j] + 1
                    scored_points <- scored_points + points
                    max_points <- max_points + 1.0
                | None -> ()

            | Hold e ->
                if not e.Missed then
                    inc holds_held

                inc holds_count

                if not e.Missed then
                    inc taps
                    tap_sum ++ e.Delta
                    tap_sumOfSq ++ e.Delta * float32 e.Delta
                    earliest_tap.Value <- min earliest_tap.Value e.Delta
                    latest_tap.Value <- max latest_tap.Value e.Delta
                    if e.Delta < 0.0f<ms / rate> then
                        inc early_taps

                match e.Judgement with
                | Some (j, points) ->
                    filtered_judgements.[j] <- filtered_judgements.[j] + 1
                    scored_points <- scored_points + points
                    max_points <- max_points + 1.0
                | None -> ()

            | Release e ->
                inc releases_count

                if not e.Missed then
                    inc releases
                    inc releases_released
                    release_sum ++ e.Delta
                    release_sumOfSq ++ e.Delta * float32 e.Delta
                    earliest_release.Value <- min earliest_release.Value e.Delta
                    latest_release.Value <- max latest_release.Value e.Delta
                    if e.Delta < 0.0f<ms / rate> then
                        inc early_releases

                match e.Judgement with
                | Some (j, points) ->
                    filtered_judgements.[j] <- filtered_judgements.[j] + 1
                    scored_points <- scored_points + points
                    max_points <- max_points + 1.0
                | None -> ()

            | GhostTap e ->
                inc ghost_taps
                match e.Judgement with
                | Some (j, points) ->
                    filtered_judgements.[j] <- filtered_judgements.[j] + 1
                    scored_points <- scored_points + points
                    max_points <- max_points + 1.0
                | None -> ()

            | DropHold -> ()
            | RegrabHold -> ()

            match ev.Combo with
            | NoChange -> ()
            | Increase -> combo <- combo + 1
            | Break _ -> combo <- 0; combo_breaks <- combo_breaks + 1

        let add_graph_points (chart_time: ChartTime) (new_graph_points_needed: int) =
            let last_point_timestamp = if graph_points.Count > 0 then graph_points.[graph_points.Count - 1].Time else 0.0f<ms>

            let mean = tap_sum.Value / float32 (max 1 taps.Value)
            let standard_deviation =
                sqrt(
                    ((tap_sumOfSq.Value / float32 (max 1 taps.Value) * 1.0f<ms / rate>)
                        - mean * mean)
                    |> float32
                )
                * 1.0f<ms / rate>
            let lamp = Lamp.calculate score_processor.Ruleset.Lamps filtered_judgements combo_breaks

            for i = 1 to new_graph_points_needed do
                let this_point_timestamp =
                    last_point_timestamp + (chart_time - last_point_timestamp) * (float32 i / float32 new_graph_points_needed)
                graph_points.Add
                    {
                        Time = this_point_timestamp
                        PointsScored = scored_points
                        MaxPointsScored = max_points
                        Combo = combo
                        Lamp = lamp
                        Mean = mean
                        StandardDeviation = standard_deviation
                        Judgements = filtered_judgements |> Array.copy
                    }

        let time_of_last_ev = if score_processor.Events.Count > 0 then score_processor.Events.[score_processor.Events.Count - 1].Time else score_processor.Duration

        for ev in score_processor.Events |> Seq.where(fun ev -> column_filter.[ev.Column]) do
            add_action ev

            let new_graph_points_needed =
                if score_processor.Events.Count <= GRAPH_POINT_COUNT then 1
                else
                    (float32 GRAPH_POINT_COUNT * ev.Time) / time_of_last_ev
                    |> ceil
                    |> int
                    |> max 0
                    |> min GRAPH_POINT_COUNT
                    |> (fun target_count -> target_count - graph_points.Count)

            add_graph_points ev.Time new_graph_points_needed

        add_graph_points time_of_last_ev 1

        let tap_mean = tap_sum.Value / float32 (max 1 taps.Value)
        let release_mean = release_sum.Value / float32 (max 1 releases.Value)

        {
            Notes = notes_hit.Value, notes_count.Value
            Holds = holds_held.Value, holds_count.Value
            Releases = releases_released.Value, releases_count.Value
            GhostTaps = ghost_taps.Value

            TapMean = tap_mean
            TapStandardDeviation =
                sqrt(
                    ((tap_sumOfSq.Value / float32 (max 1 taps.Value) * 1.0f<ms / rate>)
                     - tap_mean * tap_mean)
                    |> float32
                )
                * 1.0f<ms / rate>
            TapEarlyPercent = float early_taps.Value / float (max 1 taps.Value)
            TapRange = earliest_tap.Value, latest_tap.Value

            ReleaseMean = release_mean
            ReleaseStandardDeviation =
                sqrt(
                    ((release_sumOfSq.Value / float32 (max 1 releases.Value) * 1.0f<ms / rate>)
                     - release_mean * release_mean)
                    |> float32
                )
                * 1.0f<ms / rate>
            ReleaseEarlyPercent = float early_releases.Value / float (max 1 releases.Value)
            ReleaseRange = earliest_release.Value, latest_release.Value

            Judgements = filtered_judgements
            JudgementCount = Array.sum filtered_judgements

            MA =
                let mv = if filtered_judgements.Length > 0 then filtered_judgements.[0] else 0
                let pf = if filtered_judgements.Length > 1 then filtered_judgements.[1] else 0
                if pf = 0 then sprintf "%.1f:0" (float mv) else sprintf "%.1f:1" (float mv / float pf)

            PA =
                let pf = if filtered_judgements.Length > 1 then filtered_judgements.[1] else 0
                let gr = if filtered_judgements.Length > 2 then filtered_judgements.[2] else 0
                if gr = 0 then sprintf "%.1f:0" (float pf) else sprintf "%.1f:1" (float pf / float gr)

            GraphPoints = graph_points.ToArray()

            Accuracy = if max_points = 0.0 then 1.0 else scored_points / max_points

            ColumnFilterApplied = column_filter |> Array.forall id |> not
        }