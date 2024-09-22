open System
open Percyqaz.Common

// This project is for a bunch of loose scripts/ad-hoc testing
// Maybe it will become its own repo of fsx files

let your_script_here () =
    OsuReplayReader.read_scores()

[<EntryPoint>]
let main argv =
    Console.BufferHeight <- 32766
    Logging.Info "=== Prelude experiments/playground ==="

    your_script_here ()

    Logging.Info "Complete"
    Logging.Shutdown()
    0
