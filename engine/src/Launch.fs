namespace Percyqaz.Flux

open System
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Windowing

module Launch =

    let entry_point (config: WindowOptions, name: string, ui_root: UIEntryPoint, icon: Bitmap option) =
        
        Logging.Info(sprintf "Launching %s: %O" name (DateTime.Now.ToString()))

        Console.detect()

        let init_success =
            try
                WindowThread.init(config, name, ui_root, icon)
                true
            with err ->
                match err with
                | :? DllNotFoundException ->
                    Logging.Critical(
                        "\n====\nEngine failed to load a DLL! Please check that:\n" +
                        " - You didn't delete one of the DLLs that came with the exe\n" + 
                        " - You have VC++ redist installed - https://aka.ms/vs/17/release/vc_redist.x64.exe\n" +
                        "If the program still won't start, please contact Percyqaz!\n====",
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