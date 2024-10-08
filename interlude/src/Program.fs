﻿open System
open System.IO
open System.Threading
open System.Diagnostics
open Percyqaz.Common
open Percyqaz.Shell
open Percyqaz.Flux
open Percyqaz.Flux.Windowing
open Prelude
open Interlude
open Interlude.UI
open Interlude.Features

let launch (instance: int) =
    Logging.Verbosity <-
        if DEV_MODE then
            LoggingLevel.DEBUG
        else
            LoggingLevel.INFO

    Logging.LogFile <- Some(Path.Combine("Logs", sprintf "log-%s.txt" (DateTime.Today.ToString("yyyyMMdd"))))

    if OperatingSystem.IsWindows() then
        Process.GetCurrentProcess().PriorityClass <- ProcessPriorityClass.High

    let crash_splash =
        Utils.splash_message_picker "CrashSplashes.txt" >> (fun s -> Logging.Critical s)

    let successful_startup =
        try
            Startup.init_startup instance
            true
        with err ->
            Logging.Critical(
                "Something went wrong when loading some of the game config/data, preventing the game from opening",
                err
            )

            crash_splash ()
            Console.ReadLine() |> ignore
            false

    if successful_startup then

        Window.after_init.Add(fun () ->
            AppDomain.CurrentDomain.ProcessExit.Add(fun args -> Startup.deinit Startup.ExternalCrash crash_splash)
        )

        Window.on_file_drop.Add(Import.FileDrop.handle)

        use icon_stream = Utils.get_resource_stream ("icon.png")
        let icon = Bitmap.from_stream true icon_stream
        let result = Launch.entry_point (Options.config, "Interlude", Startup.init_window instance, icon)

        Startup.deinit (if result = Ok() then Startup.Normal else Startup.InternalCrash) crash_splash

[<EntryPoint>]
let main argv =
    let executable_location = AppDomain.CurrentDomain.BaseDirectory

    try
        Directory.SetCurrentDirectory(executable_location)
    with err ->
        Logging.Error(executable_location, err)

    if
        executable_location.Replace("\\", "/").ToLower().Contains "appdata/local/temp"
    then
        printfn "Hello ZIP FILE USER,\n\nplease EXTRACT all contents of the Interlude zip file before running the exe\notherwise it won't work or save any of your data.\n\nThanks,\nPercyqaz"
        Console.ReadLine() |> ignore
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
        printfn
            "Interlude is missing the appropriate audio library dll/so/dylib files for your platform.\n If you are a developer, info on how to fix this is at https://github.com/YAVSRG/YAVSRG#readme\n If you are not a developer, looks like you deleted a file you shouldn't have!\n Redownloading the game and extracting the zip over this folder to replace what is missing should fix it."
        Console.ReadLine() |> ignore
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

    else if m.WaitOne(TimeSpan.Zero, true) then
        launch (0)
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
        // todo: command to maximise/show Interlude window when already running
        printfn "Interlude is already running!"

    0
