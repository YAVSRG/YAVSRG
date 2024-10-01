namespace Interlude.Features.Play.Replay

open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Audio
open Percyqaz.Common
open Prelude
open Prelude.Charts.Processing
open Prelude.Gameplay.Replays
open Prelude.Gameplay.Scoring
open Interlude.UI
open Interlude.Options
open Interlude.Features.Gameplay
open Interlude.Features.Play

type private HitOverlay
    (
        rate: Rate,
        chart: ModdedChart,
        replay_data: ReplayData,
        state: PlayState,
        playfield: Playfield,
        enable: Setting<bool>
    ) =
    inherit StaticWidget(NodeType.None)

    let hit_events =
        let full_score =
            ScoreProcessor.run state.Ruleset chart.Keys (StoredReplayProvider replay_data) chart.Notes rate

        full_score.Events |> Array.ofSeq

    let mutable seek = 0

    let scroll_direction_pos: float32 -> Rect -> Rect =
        if options.Upscroll.Value then
            fun _ -> id
        else
            fun bottom ->
                fun (r: Rect) ->
                    {
                        Left = r.Left
                        Top = bottom - r.Bottom
                        Right = r.Right
                        Bottom = bottom - r.Top
                    }

    let MAX_WINDOW = state.Ruleset.LargestWindow

    override this.Init(parent) =
        state.ScoringChanged.Publish.Add(fun _ -> seek <- 0)
        base.Init parent

    override this.Draw() =

        if not enable.Value then
            ()
        else
            let draw_event (now: ChartTime) (ev: GameplayEvent<GameplayAction>) =
                let y t =
                    float32 options.HitPosition.Value
                    + (t - now) * 1.0f<rate / ms> * (options.ScrollSpeed.Value / SelectedChart.rate.Value)
                    + playfield.ColumnWidth * 0.5f

                // todo: properly match on all events

                let delta =
                    match ev.Action with
                    | Hit x -> x.Delta
                    | Hold x -> x.Delta
                    | Release x -> x.Delta
                    | _ -> 0.0f<ms / rate>

                let is_miss =
                    match ev.Action with
                    | Hit x -> x.Missed
                    | Hold x -> x.Missed
                    | Release x -> x.Missed
                    | _ -> false

                let color =
                    match ev.Action with
                    | Hit x ->
                        match x.Judgement with
                        | None -> Colors.grey_1.O2
                        | Some (i, _) -> state.Ruleset.JudgementColor i
                    | Hold x ->
                        match x.Judgement with
                        | None -> Colors.grey_1.O2
                        | Some (i, _) -> state.Ruleset.JudgementColor i
                    | Release x ->
                        match x.Judgement with
                        | None -> Colors.grey_1.O2
                        | Some (i, _) -> state.Ruleset.JudgementColor i
                    | DropHold -> Colors.pink
                    | RegrabHold -> Colors.blue
                    | GhostTap -> Colors.green

                if is_miss then
                    Rect
                        .Create(
                            playfield.Bounds.Left + playfield.ColumnPositions.[ev.Column],
                            y (ev.Time - delta * SelectedChart.rate.Value),
                            playfield.Bounds.Left
                            + playfield.ColumnPositions.[ev.Column]
                            + playfield.ColumnWidth,
                            y (ev.Time - delta * SelectedChart.rate.Value)
                        )
                        .Shrink(0.0f, -playfield.ColumnWidth * 0.5f)
                    |> scroll_direction_pos playfield.Bounds.Bottom
                    |> fun a -> Text.fill_b (Style.font, Icons.X, a, (color, Colors.black), 0.5f)
                else
                    Rect
                        .Create(
                            playfield.Bounds.Left + playfield.ColumnPositions.[ev.Column],
                            y (ev.Time - delta * SelectedChart.rate.Value),
                            playfield.Bounds.Left
                            + playfield.ColumnPositions.[ev.Column]
                            + playfield.ColumnWidth,
                            y (ev.Time - delta * SelectedChart.rate.Value)
                        )
                        .Shrink((playfield.ColumnWidth - 5.0f) * 0.75f, -2.5f)
                    |> scroll_direction_pos playfield.Bounds.Bottom
                    |> fun a -> Draw.rect a Colors.grey_2.O2

                    Rect
                        .Create(
                            playfield.Bounds.Left + playfield.ColumnPositions.[ev.Column],
                            y ev.Time,
                            playfield.Bounds.Left
                            + playfield.ColumnPositions.[ev.Column]
                            + playfield.ColumnWidth,
                            y (ev.Time - delta * SelectedChart.rate.Value)
                        )
                        .Shrink((playfield.ColumnWidth - 5.0f) * 0.5f, 0.0f)
                    |> scroll_direction_pos playfield.Bounds.Bottom
                    |> fun a -> Draw.rect a color

                    Rect
                        .Create(
                            playfield.Bounds.Left + playfield.ColumnPositions.[ev.Column],
                            y ev.Time,
                            playfield.Bounds.Left
                            + playfield.ColumnPositions.[ev.Column]
                            + playfield.ColumnWidth,
                            y ev.Time
                        )
                        .Shrink(20.0f, -2.5f)
                    |> scroll_direction_pos playfield.Bounds.Bottom
                    |> fun a -> Draw.rect a color

            let now =
                state.CurrentChartTime()
                + (if Song.playing() then Performance.frame_compensation () else 0.0f<ms>)
                + options.VisualOffset.Value * 1.0f<ms / rate> * SelectedChart.rate.Value

            while hit_events.Length - 1 > seek && hit_events.[seek + 1].Time < now - 100.0f<ms> do
                seek <- seek + 1

            let until_time =
                now
                + (1080.0f<ms / rate> + MAX_WINDOW)
                  / (options.ScrollSpeed.Value / SelectedChart.rate.Value)

            let mutable peek = seek

            while hit_events.Length - 1 > peek && hit_events.[peek].Time < until_time do
                draw_event now hit_events.[peek]
                peek <- peek + 1