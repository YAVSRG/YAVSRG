open System
open System.Threading
open System.Diagnostics
open Percyqaz.Common
open Percyqaz.Flux
open Percyqaz.Flux.Windowing
open Interlude
open Interlude.UI
open Interlude.Features
open Interlude.Features.Printerlude
open Interlude.Features.Online

let launch(instance: int) =
    Logging.Verbosity <- if Prelude.Common.DEV_MODE then LoggingLevel.DEBUG else LoggingLevel.INFO
    Logging.LogFile <- Some "log.txt"

    Process.GetCurrentProcess().PriorityClass <- ProcessPriorityClass.High

    let crashSplash = Utils.randomSplash("CrashSplashes.txt") >> (fun s -> Logging.Critical s)

    try Options.load(instance)
    with err -> Logging.Critical("Fatal error loading game config", err); crashSplash(); Console.ReadLine() |> ignore
    
    Window.afterInit.Add(fun () -> 
        Content.init Options.options.Theme.Value Options.options.Noteskin.Value
        Options.Hotkeys.init Options.options.Hotkeys
        Printerlude.init()
    )
    Window.onUnload.Add(Gameplay.save)
    Window.onFileDrop.Add(fun path -> 
        if not (Content.Noteskins.tryImport path [4; 7]) then 
            if not (Import.FileDropHandling.tryImport path) then
                Logging.Warn("Unrecognised file dropped: " + path))

    Launch.entryPoint
        (
            Options.config,
            "Interlude",
            Startup.ui_entry_point()
        )

    Options.save()
    Network.shutdown()

    Logging.Shutdown()

[<EntryPoint>]
let main argv =
    let m = new Mutex(true, "Interlude")

    if argv.Length > 0 then
        printfn "Command line arguments to Interlude not yet supported."

    elif m.WaitOne(TimeSpan.Zero, true) then
        launch(0)
        m.ReleaseMutex()

    elif Prelude.Common.DEV_MODE then
        let instances = Process.GetProcessesByName "Interlude" |> Array.length
        launch(instances - 1)
    else
        printfn "Interlude is already running."
    0
