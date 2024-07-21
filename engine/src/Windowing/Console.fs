namespace Percyqaz.Flux.Windowing

open System
open System.Runtime.InteropServices
open Percyqaz.Common

module Console =

    open System.Diagnostics
    
    [<DllImport("user32.dll")>]
    extern bool private ShowWindow(IntPtr hWindow, int32 nCmdShow)

    let mutable private hWindow = -1n

    let detect() =
        if OperatingSystem.IsWindows() then
            try
                hWindow <- Process.GetCurrentProcess().MainWindowHandle
            with err -> Logging.Debug("No console window available to hide", err)

    let hide() =
        if OperatingSystem.IsWindows() && hWindow <> -1n then
            ShowWindow(hWindow, 0x00) |> ignore

    let restore () =
        if OperatingSystem.IsWindows() && hWindow <> -1n then
            ShowWindow(hWindow, 0x09) |> ignore