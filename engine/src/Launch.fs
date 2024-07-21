namespace Percyqaz.Flux

open System
open System.Runtime.InteropServices
open OpenTK.Windowing.Common.Input
open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI

module Launch =

    let entry_point (config: Config, name: string, ui_root: Root, icon: Utils.Bitmap option) =
        Logging.Info(sprintf "Launching %s: %O" name (DateTime.Now.ToString()))

        Console.hide_console_on_successful_launch()

        let window =
            try
                let w = new Window(config, name, ui_root)

                match icon with
                | Some image ->
                    let mutable data = System.Span<_>.Empty
                    let _ = image.TryGetSinglePixelSpan(&data)

                    w.Icon <-
                        new WindowIcon(
                            new OpenTK.Windowing.Common.Input.Image(
                                image.Width,
                                image.Height,
                                MemoryMarshal.AsBytes(data).ToArray()
                            )
                        )
                | None -> ()

                Some w
            with err ->
                match err with
                | :? DllNotFoundException ->
                    Logging.Critical(
                        "\n====\nEngine failed to load a DLL! Please check that:\n - You didn't delete one of the DLLs that came with the exe\n - You have VC++ redist installed - https://aka.ms/vs/17/release/vc_redist.x64.exe\nIf the program still won't start, please contact Percyqaz!\n====",
                        err
                    )
                | _ -> Logging.Critical(name + " failed to launch", err)
                Console.ReadLine() |> ignore
                None

        if window.IsSome then
            let mutable crashed = false
            use window = window.Value

            try
                window.Run()
            with err ->
                Logging.Critical("Fatal error in window thread", err)
                crashed <- true

            window.Close()

            if crashed then
                ignore (Console.ReadLine())
