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

let launch (instance: int) =
    Logging.Verbosity <-
        if Prelude.Common.DEV_MODE then
            LoggingLevel.DEBUG
        else
            LoggingLevel.INFO

    Logging.LogFile <- Some(Path.Combine("Logs", sprintf "log-%s.txt" (DateTime.Today.ToString("yyyyMMdd"))))

    if OperatingSystem.IsWindows() then
        Process.GetCurrentProcess().PriorityClass <- ProcessPriorityClass.High

    let crash_splash =
        Utils.splash_message_picker "CrashSplashes.txt"
        >> (fun s -> Logging.Critical s)

    try
        Startup.init_startup instance
    with err ->
        Logging.Critical("Something went wrong when loading some of the game config/data, preventing the game from opening", err)
        crash_splash ()
        Console.ReadLine() |> ignore

    Window.after_init.Add(fun () -> AppDomain.CurrentDomain.ProcessExit.Add(fun args -> Startup.deinit true crash_splash))
    Window.on_file_drop.Add(Import.Import.handle_file_drop)

    use icon_stream = Utils.get_resource_stream ("icon.png")
    use icon = Utils.Bitmap.load icon_stream
    Launch.entry_point (Options.config, "Interlude", Startup.init_window instance, Some icon)

    Startup.deinit false crash_splash

[<EntryPoint>]
let main argv =
    let executable_location = AppDomain.CurrentDomain.BaseDirectory

    try
        Directory.SetCurrentDirectory(executable_location)
    with err ->
        Logging.Error(executable_location, err)

    if
        not (File.Exists "bass.dll")
        && not (File.Exists "libbass.so")
        && not (File.Exists "libbass.dylib")
    then
        printfn
            "Interlude is missing the appropriate audio library dll/so/dylib for your platform.\n If you are a developer, info on how to fix this is at https://github.com/YAVSRG/YAVSRG#readme\n If you are not a developer, looks like you deleted a file you shouldn't have!\n Redownloading the game and extracting the zip over this folder to replace what is missing should fix it."

        -1
    else

    let m = new Mutex(true, "Interlude")

    if argv.Length > 0 then

        if m.WaitOne(TimeSpan.Zero, true) then
            printfn "Error: Interlude is not running!"
            m.ReleaseMutex()

        else if argv.Length > 0 then
            match Shell.IPC.send "Interlude" (String.concat " " argv) with
            | Some success -> printfn "%s" success
            | None -> printfn "Error: Connection timed out!"

    else if m.WaitOne(TimeSpan.Zero, true)
    then
        launch (0)
        m.ReleaseMutex()

        if Utils.AutoUpdate.restart_on_exit then
            m.Dispose()

            if OperatingSystem.IsWindows() then
                let executable = Path.Combine(executable_location, "Interlude.exe")
                let launch_dir = Path.GetDirectoryName executable

                try
                    let _ =
                        Process.Start(
                            ProcessStartInfo(executable, WorkingDirectory = launch_dir, UseShellExecute = true)
                        )

                    printfn "Restarting"
                with err ->
                    printfn "Automatic restart failed :("
                    printfn "%O" err

    elif Prelude.Common.DEV_MODE then
        let instances = Process.GetProcessesByName "Interlude" |> Array.length
        launch (instances - 1)
    else
        // todo: command to maximise/show Interlude window when already running
        printfn "Interlude is already running!"

    0
