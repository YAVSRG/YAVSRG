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
        graph ms (ms_to_bpm >> sprintf "%.0f BPM") Difficulty.ms_to_jack_bpm 0.5f

    let stream () =
        let ms = seq { 1 .. 360 } |> Seq.map (fun i -> float32 i * 1.0f<ms / rate>)
        graph ms (ms_to_bpm >> sprintf "%.0f BPM") Difficulty.ms_to_stream_bpm 0.5f

    let comp () =
        let ms = seq { 0 .. 1000 } |> Seq.map (fun i -> float32 i * 1.0f<ms / rate>)
        graph ms (sprintf "%.0f ms") (fun x -> Difficulty.jack_compensation x 500.0f<ms / rate>) 0.01f

    let stam () =

        let mutable v = 0.0f
        let mutable v2 = 0.0f
        while true do
            printf "diff: "
            let diff = System.Console.ReadLine() |> float32

            let spacing = 15000.0f<ms / rate> / diff

            let freq = 1000.0f<ms / rate> / spacing |> ceil |> int

            let target = (diff * (100.0f<ms / rate> / spacing) * 2.439f)
            printfn "estimate target: %.2f" target
            printfn "estimate result: %.2f" (v + (target - v) * (1.0f - exp -0.44f))

            for i = 1 to freq do
                v <- Difficulty.stamina_func v diff spacing
                v2 <- Difficulty.stamina_func_2 v2 diff spacing
                printfn "%.2f | %.2f" v v2
            printfn "%i steps, %.1fms apart at %.2f: %.2f | %.2f" freq spacing diff v v2

    let main() =
        stam()