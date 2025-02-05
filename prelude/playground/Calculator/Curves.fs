namespace Prelude.Playground.Calculator

open Prelude
open Prelude.Charts.Processing.Difficulty

module CurveExperiments =

    let graph (values: 'T seq) (fmt: 'T -> string) (func: 'T -> float) (scale: float) =
        printfn "\n Key: # = %.1f units \n" scale

        for v in values do
            let result = func v / scale |> floor |> int
            let boxes = String.replicate result "#"
            printfn "%s: %s" (fmt v) boxes

    let ms_to_bpm (ms: float32<ms / rate>) = 15000.0f<beat ms / minute / rate> / ms
    let bpm_to_ms (bpm: float32<beat / minute>) = 15000.0f<beat ms / minute / rate> / bpm

    let jack () =
        let ms = seq { 1 .. 360 } |> Seq.map (fun i -> float32 i * 1.0f<ms / rate>)
        graph ms (ms_to_bpm >> sprintf "%.0f BPM") DifficultyRating.jack_curve 0.5

    let stream () =
        let ms = seq { 1 .. 360 } |> Seq.map (fun i -> float32 i * 1.0f<ms / rate>)
        graph ms (ms_to_bpm >> sprintf "%.0f BPM") DifficultyRating.stream_curve 0.5

    let comp () =
        let ms = seq { 0 .. 1000 } |> Seq.map (fun i -> float32 i * 1.0f<ms / rate>)
        graph ms (sprintf "%.0f ms") (fun x -> DifficultyRating.jack_compensation x 500.0f<ms / rate>) 0.01

    let main() =
        comp()