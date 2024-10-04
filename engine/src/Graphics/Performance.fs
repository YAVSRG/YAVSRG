namespace Percyqaz.Flux.Graphics

open Percyqaz.Common

module Performance =

    let mutable framecount_tickcount = (0, 1L)
    let mutable visual_latency_lo = 0.0
    let mutable visual_latency_hi = 0.0
    let mutable update_time = 0.0
    let mutable draw_time = 0.0
    let mutable elapsed_ms = 0.0

    let mutable frame_compensation: unit -> float32<ms / rate> = K 0.0f<ms / rate>
