namespace Prelude.Playground.Calculator

open Prelude
open Prelude.Calculator

module Totals =

    let RATES = [0.8f<rate>; 0.9f<rate>; 1.0f<rate>; 1.1f<rate>; 1.2f<rate>]

    let calculate_rates (label: string) (pattern: _) =
        for rate in RATES do
            [|1f; 2f; 3f|]
            |> Array.map (fun duration ->
                let rating = Difficulty.calculate (rate, ChartBuilder.repeated_pattern 4 (duration * 60_000.0f<ms>) pattern)
                rating.Overall
            )
            |> printfn "'%s' on %.1fx: %A" label rate

            printfn ""

    let main() =
        [| 0b1111us |] |> calculate_rates "Quad wall"
        [| 0b1100us |] |> calculate_rates "1H Jump wall"
        [| 0b1001us |] |> calculate_rates "2H Jump wall"
        [|
            0b1111us
            0b1011us
            0b1111us
            0b1101us
            0b1111us
            0b0111us
            0b1111us
            0b1110us
        |]
        |> calculate_rates "Quad-Hand"
        [|
            0b1111us
            0b1010us
            0b0111us
            0b1001us

            0b1110us
            0b0011us
            0b1011us
            0b0110us

            0b1101us
            0b1110us
            0b1011us
            0b0111us

            0b1100us
            0b1010us
            0b0111us
            0b1001us
        |]
        |> calculate_rates "Chordjacks"