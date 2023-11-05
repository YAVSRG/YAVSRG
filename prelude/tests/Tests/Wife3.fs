namespace Prelude.Test

open System
open Percyqaz.Common
open Prelude.Gameplay.RulesetUtils

module Wife3 =

    let main () =
        Logging.Info "Wife3 tests ..."

        let ran = Random()

        let delta = 6f<ms>
        let base_value = wife_curve 4 delta

        let note_rounding_error_value =
            { 1 .. 1000 }
            |> Seq.map (fun _ -> wife_curve 4 (delta + (-0.5 + ran.NextDouble() |> float32 |> fun x -> x * 1.0f<ms>)))
            |> Seq.average

        printfn "Centered value: %.16f%%\nWith variation: %.16f%%" (base_value * 100.0) (note_rounding_error_value * 100.0)


