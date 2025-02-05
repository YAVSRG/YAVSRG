open System
open System.IO
open Percyqaz.Common
open Prelude.Playground.Calculator

// This project is for a bunch of loose scripts/ad-hoc testing
// Maybe it will become its own repo of fsx files

let your_script_here () =
    //OsuReplayGenerator.run_experiment()
    //OsuReplayReader.read_scores()
    //Endless.test()
    CurveExperiments.main()

[<EntryPoint>]
let main argv =
    Console.BufferHeight <- 32766
    Logging.LogFile <- Some(Path.Combine("Logs", sprintf "log-%s.txt" (DateTime.Today.ToString("yyyyMMdd"))))

    your_script_here ()

    Logging.Shutdown()
    0