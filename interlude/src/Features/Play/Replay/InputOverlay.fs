namespace Interlude.Features.Play.Replay

open Percyqaz.Flux.Audio
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay.Replays
open Interlude.Options
open Interlude.Features.Gameplay
open Interlude.Features.Play

type private InputOverlay(keys: int, replay: Replay, state: PlayState, playfield: Playfield)
    =
    inherit StaticWidget(NodeType.None)

    let replay_frames = Replay.to_array replay
    let mutable seek = 0
    let mutable last_time = 0.0f<ms>
    let keys_down = Array.zeroCreate keys
    let keys_times = Array.zeroCreate keys

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

    override this.Init(parent) =
        state.OnScoringChanged(fun _ -> seek <- 0) |> ignore
        base.Init(parent)

    override this.Draw() =

        if show_input_overlay.Value then
            let draw_press (k, now: ChartTime, start: ChartTime, finish: ChartTime) =
                let y t =
                    options.HitPosition.Value
                    + (t - now) * (options.ScrollSpeed.Value / SelectedChart.rate.Value)
                    + playfield.ColumnWidth * 0.5f

                Rect
                    .FromEdges(
                        playfield.Bounds.Left + playfield.ColumnPositions.[k],
                        y start,
                        playfield.Bounds.Left + playfield.ColumnPositions.[k] + playfield.ColumnWidth,
                        y finish
                    )
                    .Shrink(20.0f, 0.0f)
                |> scroll_direction_pos playfield.Bounds.Bottom
                |> fun a -> Render.rect a Colors.grey_2.O2

            let now =
                state.CurrentChartTime() +
                (GameThread.frame_compensation () + options.VisualOffset.Value) * Song.playback_rate()

            while replay_frames.Length - 1 > seek && replay_frames.[seek + 1].Time < now - 100.0f<ms> do
                seek <- seek + 1

            let until_time =
                now + 1080.0f / (options.ScrollSpeed.Value / SelectedChart.rate.Value)

            let mutable peek = seek
            let replay_frame = replay_frames.[peek]

            for k = 0 to keys - 1 do
                if replay_frame.PressedKeys.Contains(k) then
                    keys_down.[k] <- true
                    keys_times.[k] <- replay_frame.Time
                else
                    keys_down.[k] <- false

            while replay_frames.Length - 1 > peek && replay_frames.[peek].Time < until_time do
                let replay_frame = replay_frames.[peek]

                for k = 0 to keys - 1 do
                    if replay_frame.PressedKeys.Contains(k) then
                        if not keys_down.[k] then
                            keys_down.[k] <- true
                            keys_times.[k] <- replay_frame.Time
                    else if keys_down.[k] then
                        keys_down.[k] <- false
                        draw_press (k, now, keys_times.[k], replay_frame.Time)

                peek <- peek + 1

            for k = 0 to keys - 1 do
                if keys_down.[k] then
                    draw_press (k, now, keys_times.[k], until_time)

    override this.Update (elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        let time = state.CurrentChartTime()
        if time < last_time then
            while seek > 0 && replay_frames.[seek].Time > time do
                seek <- seek - 1
        last_time <- state.CurrentChartTime()