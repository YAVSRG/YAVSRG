namespace Percyqaz.Flux

open System
open System.Runtime.InteropServices
open OpenTK.Windowing.Common.Input
open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI

module Launch =

    let entry_point (config: WindowingUserOptions, name: string, ui_root: Root, icon: Utils.Bitmap option) =
        
        WindowThread.init(config.Snapshot, name, ui_root)
        WindowThread.run()

        Ok()

        //Logging.Info(sprintf "Launching %s: %O" name (DateTime.Now.ToString()))

        //Console.detect()

        //let window =
        //    try
        //        let w = new Window(config, name, ui_root)

        //        match icon with
        //        | Some image ->
        //            let mutable data = System.Span<_>.Empty
        //            let _ = image.TryGetSinglePixelSpan(&data)

        //            w.Icon <-
        //                new WindowIcon(
        //                    new OpenTK.Windowing.Common.Input.Image(
        //                        image.Width,
        //                        image.Height,
        //                        MemoryMarshal.AsBytes(data).ToArray()
        //                    )
        //                )
        //        | None -> ()

        //        Some w
        //    with err ->
        //        match err with
        //        | :? DllNotFoundException ->
        //            Logging.Critical(
        //                "\n====\nEngine failed to load a DLL! Please check that:\n - You didn't delete one of the DLLs that came with the exe\n - You have VC++ redist installed - https://aka.ms/vs/17/release/vc_redist.x64.exe\nIf the program still won't start, please contact Percyqaz!\n====",
        //                err
        //            )
        //        | _ -> Logging.Critical(name + " failed to launch", err)
        //        Console.ReadLine() |> ignore
        //        None

        //if window.IsSome then
        //    let mutable crashed = false
        //    use window = window.Value
            
        //    Window.after_init.Add (fun () -> Console.hide())

        //    try
        //        crashed <- window.Run() <> Ok()
        //    with err ->
        //        Logging.Critical("Fatal error in window/input thread", err)
        //        crashed <- true

        //    if crashed then Error() else Ok()

        //else Error()