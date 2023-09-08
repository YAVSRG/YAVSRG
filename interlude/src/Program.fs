open System
open System.Threading
open System.Diagnostics
open Percyqaz.Common
open Percyqaz.Shell
open Percyqaz.Flux
open Percyqaz.Flux.Windowing
open Interlude
open Interlude.UI
open Interlude.Features
open Interlude.Features.Import
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
        Printerlude.init(instance)
    )
    Window.onUnload.Add(Gameplay.save)
    Window.onFileDrop.Add(fun path -> 
        if not (Content.Noteskins.tryImport path [4; 7]) then 
            if not (Import.dropFile path) then
                Logging.Warn("Unrecognised file dropped: " + path))

    use icon_stream = Utils.getResourceStream("icon.png")
    use icon = Utils.Bitmap.load icon_stream

    Launch.entryPoint
        (
            Options.config,
            "Interlude",
            Startup.ui_entry_point(),
            Some icon
        )

    Options.save()
    Network.shutdown()
    Printerlude.shutdown()

    Logging.Shutdown()

[<EntryPoint>]
let main argv =
    let m = new Mutex(true, "Interlude")

    if argv.Length > 0 then

        if m.WaitOne(TimeSpan.Zero, true) then
            printfn "Interlude is not running!"
            m.ReleaseMutex()

        else
            if argv.Length > 0 then
                String.concat " " argv
                |> Shell.IPC.send "Interlude"
                |> printfn "%s"

    else

        if m.WaitOne(TimeSpan.Zero, true) then
            launch(0)
            m.ReleaseMutex()

        elif Prelude.Common.DEV_MODE then
            let instances = Process.GetProcessesByName "Interlude" |> Array.length
            launch(instances - 1)
        else
            // todo: command to maximise/show Interlude window when already running
            printfn "Interlude is already running!"
    0
