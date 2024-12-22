namespace Percyqaz.Flux

open System
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Windowing

module Launch =

    let entry_point (config: unit -> WindowOptions, name: string, init_thunk: unit -> UIEntryPoint, icon: Bitmap option) =

        try
            let config = config()
            Ok config
        with err ->
            Logging.Critical "Failed to load window config: %O" err
            Error()
        |> function
        | Error() -> Error()
        | Ok config ->

        Logging.Info "Launching %s: %O" name (DateTime.Now.ToString())

        let init_success =
            try
                WindowThread.init(config, name, init_thunk, icon)
                true
            with err ->
                match err with
                | :? DllNotFoundException ->
                    Logging.Critical
                        """====\nEngine failed to load a DLL! Please check that:
 - You didn't delete one of the DLLs that came with the exe\n" +
 - You have VC++ redist installed - https://aka.ms/vs/17/release/vc_redist.x64.exe\n" +
If the program still won't start, please contact Percyqaz!\n====\n%O""" err
                | _ -> Logging.Critical "%s failed to launch: %O" name err
                false

        if init_success then
            try
                WindowThread.run()
            with err ->
                Logging.Critical "Fatal error in window/input thread: %O" err
                Error()
        else Error()