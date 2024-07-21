namespace Percyqaz.Flux.Windowing

open System
open System.Runtime.InteropServices
open Percyqaz.Common

module Console =

    open System.Diagnostics
    
    [<DllImport("user32.dll")>]
    extern bool ShowWindow(IntPtr hWindow, int32 nCmdShow)

    let hide_console_on_successful_launch() =
        if OperatingSystem.IsWindows() then
            try
                let hWindow = Process.GetCurrentProcess().MainWindowHandle
                Window.after_init.Add(fun () -> ShowWindow(hWindow, 0) |> ignore)
            with err -> 
                Logging.Error("Error hiding console window", err)