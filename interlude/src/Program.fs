open System
open System.IO
open System.Threading
open System.Diagnostics
open Percyqaz.Common
open Percyqaz.Shell
open Percyqaz.Flux
open Percyqaz.Flux.Windowing
open Prelude
open Interlude
open Interlude.Options
open Interlude.UI
open Interlude.Features

let crash_text_file (message: string) =
    let path = Path.ChangeExtension(Path.GetTempFileName(), ".txt")
    File.WriteAllText(path, message)
    open_directory(path)

let launch (instance: int) : unit =
    Logging.Verbosity <- if DEV_MODE then LoggingLevel.DEBUG else LoggingLevel.INFO

    Logging.LogFile <- Some(Path.Combine("Logs", sprintf "log-%s.txt" (DateTime.Today.ToString("yyyyMMdd"))))

    if OperatingSystem.IsWindows() then
        Process.GetCurrentProcess().PriorityClass <- ProcessPriorityClass.High

    let show_crash_splash =
        Utils.splash_message_picker "CrashSplashes.txt" >> Logging.Critical "%s"

    let init () =
        let ui_root = Startup.init instance
        AppDomain.CurrentDomain.ProcessExit.Add(fun _ -> Startup.deinit Startup.ExternalCrash show_crash_splash)
        ui_root :> UIEntryPoint

    WindowThread.on_file_drop.Add(fun paths -> if paths.Length <> 1 then Logging.Error "Multiple file drops not supported" else Import.FileDrop.handle paths.[0])

    let icon =
        use icon_stream = Utils.get_resource_stream "icon.png"
        Bitmap.from_stream true icon_stream

    let result = Launch.entry_point (Options.load_window_config instance, Updates.version, init, icon)

    Startup.deinit (if result = Ok() then Startup.Normal else Startup.InternalCrash) show_crash_splash

[<EntryPoint>]
let main (argv: string array) : int =
    let executable_location = AppDomain.CurrentDomain.BaseDirectory
    Directory.SetCurrentDirectory(executable_location)

    if
        executable_location.Replace("\\", "/").ToLower().Contains "appdata/local/temp"
    then
        crash_text_file "Hello ZIP FILE USER,\n\nplease EXTRACT all contents of the Interlude zip file before running the exe\notherwise it won't work or save any of your data.\n\nThanks,\nPercyqaz"
        -1
    elif
        (
            not (File.Exists "bass.dll")
            && not (File.Exists "libbass.so")
            && not (File.Exists "libbass.dylib")
        )
        ||
        (
            not (File.Exists "bass_fx.dll")
            && not (File.Exists "libbass_fx.so")
            && not (File.Exists "libbass_fx.dylib")
        )
    then
        crash_text_file "Interlude is missing the appropriate audio library dll/so/dylib files for your platform.\n If you are a developer, info on how to fix this is at https://github.com/YAVSRG/YAVSRG#readme\n If you are not a developer, looks like you deleted a file you shouldn't have!\n Redownloading the game and extracting the zip over this folder to replace what is missing should fix it."
        -1
    else

    let m = new Mutex(true, "Interlude")

    if argv.Length > 0 then

        if m.WaitOne(TimeSpan.Zero, true) then
            printfn "Error: Interlude is not running!"
            m.ReleaseMutex()

        else
            match Shell.IPC.send "Interlude" (String.concat " " argv) with
            | Some success -> printfn "%s" success
            | None -> printfn "Error: Connection timed out!"

    else if m.WaitOne(TimeSpan.Zero, true) then
        launch 0
        m.ReleaseMutex()

        if Updates.restart_on_exit then
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

    elif DEV_MODE then
        let instances = Process.GetProcessesByName "Interlude" |> Array.length
        launch (instances - 1)
    else
        match Shell.IPC.send "Interlude" "focus" with
        | Some success -> printfn "%s" success
        | None -> printfn "Error: Connection timed out!"

    Environment.Exit(0)
    0