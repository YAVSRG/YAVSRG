open System
open System.Threading
open System.Diagnostics
open Percyqaz.Common
open Percyqaz.Flux
open Interlude
open Interlude.UI

[<EntryPoint>]
let main argv =
    let m = new Mutex(true, "Interlude")

    let crashSplash = Utils.randomSplash("CrashSplashes.txt") >> (fun s -> Logging.Critical s)

    // Ensure only one instance of Interlude is running
    if m.WaitOne(TimeSpan.Zero, true) then
        Process.GetCurrentProcess().PriorityClass <- ProcessPriorityClass.High

        try
            Options.load()
        with err -> Logging.Critical("Fatal error loading game config", err); crashSplash(); Console.ReadLine() |> ignore

        Launch.entryPoint
            (
                Options.config,
                "Interlude",
                Startup.Root()
            )

        m.ReleaseMutex()
    else
        // todo: code that sends data to the running process to reappear if hidden
        ()
    0
