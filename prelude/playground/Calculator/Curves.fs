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
        graph ms (ms_to_bpm >> sprintf "%.0f BPM") NoteDifficulty.ms_to_jack_bpm 0.5f

    let stream () =
        let ms = seq { 1 .. 360 } |> Seq.map (fun i -> float32 i * 1.0f<ms / rate>)
        graph ms (ms_to_bpm >> sprintf "%.0f BPM") NoteDifficulty.ms_to_stream_bpm 0.5f

    let comp () =
        let ms = seq { 0 .. 1000 } |> Seq.map (fun i -> float32 i * 1.0f<ms / rate>)
        graph ms (sprintf "%.0f ms") (fun x -> NoteDifficulty.jack_compensation x 500.0f<ms / rate>) 0.01f

    let stam () =

        let mutable v2 = 0.0f
        while true do
            printf "diff: "
            let diff = System.Console.ReadLine() |> float32

            let spacing = 15000.0f<ms / rate> / diff

            let freq = 1000.0f<ms / rate> / spacing |> ceil |> int

            let target = (diff * (100.0f<ms / rate> / spacing) * 2.439f)
            printfn "estimate target: %.2f" target
            printfn "estimate result: %.2f" (v2 + (target - v2) * (1.0f - exp -0.44f))

            for i = 1 to freq do
                v2 <- Strain.strain_burst v2 diff spacing
                printfn "%.2f" v2
            printfn "%i steps, %.1fms apart at %.2f: %.2f" freq spacing diff v2

    let trill_vs_jack () =
        for bpm in [80.0f; 90.0f; 100.0f; 110.0f; 120.0f; 130.0f; 140.0f; 150.0f; 160.0f; 170.0f; 180.0f; 190.0f; 200.0f] do
            let ms = 15000.0f<ms / rate> / bpm
            let jack_curve = NoteDifficulty.ms_to_jack_bpm ms
            let stream_curve = NoteDifficulty.ms_to_stream_bpm (ms / 2.0f)
            let roll_curve_a = NoteDifficulty.ms_to_stream_bpm (ms / 4.0f)
            let roll_curve_b = NoteDifficulty.ms_to_stream_bpm (ms * 3.0f / 4.0f)
            let jack_value = { J = jack_curve; SL = 0.0f; SR = 0.0f }.Total
            let trill_value = { J = jack_curve; SL = stream_curve; SR = 0.0f }.Total
            let bracket_value = { J = jack_curve; SL = stream_curve; SR = stream_curve }.Total
            let roll_value_a = { J = jack_curve; SL = roll_curve_a; SR = 0.0f }.Total
            let roll_value_b = { J = jack_curve; SL = roll_curve_b; SR = 0.0f }.Total

            printfn "Notes in a %.0f longjack are valued like %.0f longjacks" bpm jack_value
            printfn "Notes in a %.0f trill (two antiphase %.0f longjacks) are valued like %.0f longjacks" (bpm * 2.0f) bpm trill_value
            printfn "Notes in a %.0f bracket are valued like %.0f longjacks" (bpm * 2.0f) bracket_value
            printfn "Notes in a %.0f roll are valued like %.0f / %.0f longjacks" (bpm * 4.0f) roll_value_a roll_value_b
            printfn "--"

    let scales () =
        for i = 80 to 100 do
            let acc = float32 i / 100.0f
            printfn "T @ %i%%: %.2f" i (Performance.tech_curve acc)
            printfn "P @ %i%%: %.2f" i (Performance.physical_curve acc)

    let main() =
        scales()