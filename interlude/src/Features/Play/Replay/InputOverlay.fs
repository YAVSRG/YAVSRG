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

type private InputOverlay(keys: int, replay_data: ReplayData, state: PlayState, playfield: Playfield)
    =
    inherit StaticWidget(NodeType.None)

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
        state.ScoringChanged.Publish.Add(fun _ -> seek <- 0)
        base.Init parent

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

            while replay_data.Length - 1 > seek
                  && let struct (t, _) = replay_data.[seek + 1] in
                     t < now - 100.0f<ms> do
                seek <- seek + 1

            let until_time =
                now + 1080.0f / (options.ScrollSpeed.Value / SelectedChart.rate.Value)

            let mutable peek = seek
            let struct (t, b) = replay_data.[peek]

            for k = 0 to keys - 1 do
                if Bitmask.has_key k b then
                    keys_down.[k] <- true
                    keys_times.[k] <- t
                else
                    keys_down.[k] <- false

            while replay_data.Length - 1 > peek
                  && let struct (t, _) = replay_data.[peek] in
                     t < until_time do
                let struct (t, b) = replay_data.[peek]

                for k = 0 to keys - 1 do
                    if Bitmask.has_key k b then
                        if not keys_down.[k] then
                            keys_down.[k] <- true
                            keys_times.[k] <- t
                    else if keys_down.[k] then
                        keys_down.[k] <- false
                        draw_press (k, now, keys_times.[k], t)

                peek <- peek + 1

            for k = 0 to keys - 1 do
                if keys_down.[k] then
                    draw_press (k, now, keys_times.[k], until_time)

    override this.Update (elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        let time = state.CurrentChartTime()
        if time < last_time then
            while seek > 0 && let struct (t, _) = replay_data.[seek] in t > time do
                seek <- seek - 1
        last_time <- state.CurrentChartTime()