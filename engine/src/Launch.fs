namespace Percyqaz.Flux

open System
open System.Runtime.InteropServices
open OpenTK.Windowing.Common.Input
open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI

module Launch =

    let entry_point (config: WindowOptions, name: string, ui_root: UIEntryPoint, icon: Utils.Bitmap option) =
        
        Logging.Info(sprintf "Launching %s: %O" name (DateTime.Now.ToString()))

        Console.detect()

        let init_success =
            try
                WindowThread.init(config, name, ui_root, icon)
                true

                //match icon with
                //| Some image ->
                //    let mutable data = System.Span<_>.Empty
                //    let _ = image.TryGetSinglePixelSpan(&data)

                //    w.Icon <-
                //        new WindowIcon(
                //            new OpenTK.Windowing.Common.Input.Image(
                //                image.Width,
                //                image.Height,
                //                MemoryMarshal.AsBytes(data).ToArray()
                //            )
                //        )
                //| None -> ()
            with err ->
                match err with
                | :? DllNotFoundException ->
                    Logging.Critical(
                        "\n====\nEngine failed to load a DLL! Please check that:\n - You didn't delete one of the DLLs that came with the exe\n - You have VC++ redist installed - https://aka.ms/vs/17/release/vc_redist.x64.exe\nIf the program still won't start, please contact Percyqaz!\n====",
                        err
                    )
                | _ -> Logging.Critical(name + " failed to launch", err)
                Console.ReadLine() |> ignore
                false

        if init_success then
            let mutable crashed = false
            GameThread.after_init.Add (fun () -> Console.hide())

            try
                crashed <- WindowThread.run() <> Ok()
            with err ->
                Logging.Critical("Fatal error in window/input thread", err)
                crashed <- true

            if crashed then Error() else Ok()

        else Error()