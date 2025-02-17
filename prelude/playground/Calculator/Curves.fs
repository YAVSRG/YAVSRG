namespace Prelude.Playground.Calculator

open Prelude
open Prelude.Calculator

module CurveExperiments =

    let graph (values: 'T seq) (fmt: 'T -> string) (func: 'T -> float32) (scale: float32) =
        printfn "\n Key: # = %.1f units \n" scale

        for v in values do
            let result = func v / scale |> floor |> int
            let boxes = String.replicate result "#"
            printfn "%s: %s" (fmt v) boxes

    let ms_to_bpm (ms: float32<ms / rate>) = 15000.0f<beat ms / minute / rate> / ms
    let bpm_to_ms (bpm: float32<beat / minute>) = 15000.0f<beat ms / minute / rate> / bpm

    let jack () =
        let ms = seq { 1 .. 360 } |> Seq.map (fun i -> float32 i * 1.0f<ms / rate>)
        graph ms (ms_to_bpm >> sprintf "%.0f BPM") DifficultyRating.jack_curve 0.5f

    let stream () =
        let ms = seq { 1 .. 360 } |> Seq.map (fun i -> float32 i * 1.0f<ms / rate>)
        graph ms (ms_to_bpm >> sprintf "%.0f BPM") DifficultyRating.stream_curve 0.5f

    let comp () =
        let ms = seq { 0 .. 1000 } |> Seq.map (fun i -> float32 i * 1.0f<ms / rate>)
        graph ms (sprintf "%.0f ms") (fun x -> DifficultyRating.jack_compensation x 500.0f<ms / rate>) 0.01f

    let stam () =
        let mutable v = 0.1f
        for i = 1 to 10 do
            for _ = 1 to 10 do
                v <- DifficultyRating.stamina_func v 10.0f 10.0f<ms / rate>
            printfn "%i iterations: %.2f" (i * 10) v
            let instant_extra = DifficultyRating.stamina_func v 10.0f 0.0f<ms / rate>
            printfn "instant extra: %.2f" instant_extra
            printfn "ratio %.2f / %.2f" (instant_extra / v) (v / instant_extra)

    let main() =
        stam()