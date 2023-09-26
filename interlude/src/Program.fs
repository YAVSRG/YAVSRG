open System
open System.IO
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
    Logging.LogFile <- Some (Path.Combine("Logs", sprintf "log-%s.txt" (DateTime.Today.ToString("yyyyMMdd"))))

    if Environment.OSVersion.Platform = PlatformID.Win32NT then
        Process.GetCurrentProcess().PriorityClass <- ProcessPriorityClass.High

    let crashSplash = Utils.randomSplash("CrashSplashes.txt") >> (fun s -> Logging.Critical s)

    try Options.load(instance)
    with err -> Logging.Critical("Fatal error loading game config", err); crashSplash(); Console.ReadLine() |> ignore
    
    let mutable has_shutdown = false
    let shutdown(unexpected) =
        if has_shutdown then () else
        Gameplay.save()
        Options.save()
        Network.shutdown()
        //DiscordRPC.shutdown()
        Printerlude.shutdown()
        if unexpected then 
            crashSplash()
            Logging.Critical("The game crashed or quit abnormally, but was able to shut down correctly")
        Logging.Shutdown()
    
    Window.afterInit.Add(fun () -> 
        Content.init Options.options.Theme.Value Options.options.Noteskin.Value
        Options.Hotkeys.init Options.options.Hotkeys
        Printerlude.init(instance)
        //DiscordRPC.init()

        AppDomain.CurrentDomain.ProcessExit.Add (fun args -> shutdown(true))
    )
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

    shutdown(false)

[<EntryPoint>]
let main argv =
    if not (File.Exists("bass.dll")) && not (File.Exists("libbass.iso") && not (File.Exists("libbass.dylib"))) then
        printfn "Looks like Interlude was launched from the wrong starting directory!"
        -1
    else

    let m = new Mutex(true, "Interlude")

    if argv.Length > 0 then

        if m.WaitOne(TimeSpan.Zero, true) then
            printfn "Error: Interlude is not running!"
            m.ReleaseMutex()

        else
            if argv.Length > 0 then
                match Shell.IPC.send "Interlude" (String.concat " " argv) with
                | Some success -> printfn "%s" success
                | None -> printfn "Error: Connection timed out!"

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
