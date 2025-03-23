namespace Interlude.Features.Play.Replay

open Percyqaz.Flux.Audio
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude
open Prelude.Mods
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
        playfield: Playfield
    ) =
    inherit StaticWidget(NodeType.None)

    let hit_events =
        let full_score =
            ScoreProcessor.run state.Ruleset chart.Keys (StoredReplayProvider replay_data) chart.Notes rate

        full_score.Events |> Array.ofSeq

    let mutable seek = 0
    let mutable last_time = 0.0f<ms>

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
    let GHOST_TAP_ANNOTATION = %"replay.ghost_tap"
    let DROP_HOLD_ANNOTATION = %"replay.drop_hold"
    let REGRAB_HOLD_ANNOTATION = %"replay.regrab_hold"

    let draw_delta (column: int) (action_y: float32) (note_y: float32) (color: Color) : unit =
        Rect
            .FromEdges(
                playfield.Bounds.Left + playfield.ColumnPositions.[column],
                note_y,
                playfield.Bounds.Left
                + playfield.ColumnPositions.[column]
                + playfield.ColumnWidth,
                note_y
            )
            .Shrink((playfield.ColumnWidth - 5.0f) * 0.75f, -2.5f)
        |> scroll_direction_pos playfield.Bounds.Bottom
        |> fun a -> Render.rect a Colors.grey_2.O2

        Rect
            .FromEdges(
                playfield.Bounds.Left + playfield.ColumnPositions.[column],
                action_y,
                playfield.Bounds.Left
                + playfield.ColumnPositions.[column]
                + playfield.ColumnWidth,
                note_y
            )
            .Shrink((playfield.ColumnWidth - 5.0f) * 0.5f, 0.0f)
        |> scroll_direction_pos playfield.Bounds.Bottom
        |> fun a -> Render.rect a color

        Rect
            .FromEdges(
                playfield.Bounds.Left + playfield.ColumnPositions.[column],
                action_y,
                playfield.Bounds.Left
                + playfield.ColumnPositions.[column]
                + playfield.ColumnWidth,
                action_y
            )
            .Shrink(20.0f, -2.5f)
        |> scroll_direction_pos playfield.Bounds.Bottom
        |> fun a -> Render.rect a color

    let label_before (column: int) (y: float32) (text: string) (color: Color) : unit =
        if show_hit_overlay_labels.Value then
            Rect
                .FromEdges(
                    playfield.Bounds.Left + playfield.ColumnPositions.[column],
                    y,
                    playfield.Bounds.Left
                    + playfield.ColumnPositions.[column]
                    + playfield.ColumnWidth,
                    y
                )
                .Shrink(0.0f, -playfield.ColumnWidth * 0.5f).SlicePercentT(0.45f).ShrinkPercentT(0.5f)
            |> scroll_direction_pos playfield.Bounds.Bottom
            |> fun a -> Text.fill_b (Style.font, text, a, (color, Colors.black), 0.5f)

    let label_after (column: int) (y: float32) (text: string) (color: Color) : unit =
        if show_hit_overlay_labels.Value then
            Rect
                .FromEdges(
                    playfield.Bounds.Left + playfield.ColumnPositions.[column],
                    y,
                    playfield.Bounds.Left
                    + playfield.ColumnPositions.[column]
                    + playfield.ColumnWidth,
                    y
                )
                .Shrink(0.0f, -playfield.ColumnWidth * 0.5f).SlicePercentB(0.45f).ShrinkPercentB(0.5f)
            |> scroll_direction_pos playfield.Bounds.Bottom
            |> fun a -> Text.fill_b (Style.font, text, a, (color, Colors.black), 0.5f)

    let draw_icon (column: int) (y: float32) (icon: string) (color: Color) : unit =
        Rect
            .FromEdges(
                playfield.Bounds.Left + playfield.ColumnPositions.[column],
                y,
                playfield.Bounds.Left
                + playfield.ColumnPositions.[column]
                + playfield.ColumnWidth,
                y
            )
            .Shrink(0.0f, -playfield.ColumnWidth * 0.5f).ShrinkPercent(0.15f)
        |> scroll_direction_pos playfield.Bounds.Bottom
        |> fun a -> Text.fill_b (Style.font, icon, a, (color, Colors.black), 0.5f)

    let draw_event (now: ChartTime) (ev: GameplayEvent) : unit =
        let ms_to_y (time: Time) =
            options.HitPosition.Value
            + (time - now) * (options.ScrollSpeed.Value / SelectedChart.rate.Value)
            + playfield.ColumnWidth * 0.5f

        match ev.Action with

        | Hit x
        | Hold x ->
            let color =
                match x.Judgement with
                | None -> Colors.grey_1.O2
                | Some (i, _) -> state.Ruleset.JudgementColor i

            let note_y = ms_to_y (ev.Time - x.Delta * SelectedChart.rate.Value)

            if x.Missed then
                draw_icon ev.Column note_y Icons.X color
            else
                let action_y = ms_to_y ev.Time
                draw_delta ev.Column action_y note_y color

                if x.Delta < 0.0f<ms / rate> then
                    label_before ev.Column action_y (sprintf "%.1fms" x.Delta) color
                else
                    label_after ev.Column action_y (sprintf "+%.1fms" x.Delta) color

        | Release x ->
            let color =
                match x.Judgement with
                | None -> Colors.grey_1.O2
                | Some (i, _) -> state.Ruleset.JudgementColor i

            let note_y = ms_to_y (ev.Time - x.Delta * SelectedChart.rate.Value)

            if x.Missed then
                draw_icon ev.Column note_y Icons.X color
            else
                let action_y = ms_to_y ev.Time
                draw_delta ev.Column action_y note_y color

                if x.Delta < 0.0f<ms / rate> then
                    label_before ev.Column action_y (sprintf "%s  %.1fms" Icons.CHEVRON_UP x.Delta) color
                else
                    label_after ev.Column action_y (sprintf "%s  +%.1fms" Icons.CHEVRON_UP x.Delta) color

        | GhostTap x ->
            let color =
                match x.Judgement with
                | None -> Colors.red_accent.O2
                | Some (i, _) -> state.Ruleset.JudgementColor i
            let action_y = ms_to_y ev.Time

            draw_delta ev.Column action_y action_y color
            label_after ev.Column action_y (sprintf "%s  %s" Icons.ARROW_DOWN GHOST_TAP_ANNOTATION) color

        | DropHold ->
            let color = Colors.red_accent.O2
            let action_y = ms_to_y ev.Time

            draw_delta ev.Column action_y action_y color
            label_after ev.Column action_y (sprintf "%s  %s" Icons.CHEVRONS_UP DROP_HOLD_ANNOTATION) color

        | RegrabHold ->
            let color = Colors.red_accent.O2
            let action_y = ms_to_y ev.Time

            draw_delta ev.Column action_y action_y color
            label_after ev.Column action_y (sprintf "%s  %s" Icons.CHEVRONS_DOWN REGRAB_HOLD_ANNOTATION) color

    override this.Init(parent) =
        state.ScoringChanged.Publish.Add(fun _ -> seek <- 0)
        base.Init parent

    override this.Draw() =
        if show_hit_overlay.Value then
            let now =
                state.CurrentChartTime() +
                (GameThread.frame_compensation () + options.VisualOffset.Value) * Song.playback_rate()

            while hit_events.Length - 1 > seek && hit_events.[seek + 1].Time < now - 100.0f<ms> do
                seek <- seek + 1

            let until_time =
                now + 1080.0f / (options.ScrollSpeed.Value / SelectedChart.rate.Value) + MAX_WINDOW * SelectedChart.rate.Value

            let mutable peek = seek

            while hit_events.Length - 1 > peek && hit_events.[peek].Time < until_time do
                draw_event now hit_events.[peek]
                peek <- peek + 1

    override this.Update (elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        let time = state.CurrentChartTime()
        if time < last_time then
            while seek > 0 && hit_events.[seek].Time > time do
                seek <- seek - 1
        last_time <- state.CurrentChartTime()