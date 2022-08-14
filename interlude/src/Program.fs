open System
open System.Threading
open System.Diagnostics
open Percyqaz.Common
open Percyqaz.Flux
open Percyqaz.Flux.Windowing
open Interlude
open Interlude.UI
open Interlude.Features.Printerlude

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
        
        Window.onLoad.Add(fun () -> 
            Content.init Options.options.Theme.Value Options.options.Noteskin.Value
            Options.Hotkeys.init Options.options.Hotkeys
            Printerlude.init()
        )

        Launch.entryPoint
            (
                Options.config,
                "Interlude",
                Startup.Root()
            )

        Options.save()

        m.ReleaseMutex()
    else
        // todo: code that sends data to the running process to reappear if hidden
        ()
    0
