namespace Percyqaz.Flux

open System
open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
    
module Launch =

    let entryPoint (config: Config, name: string, root: Root) =
        Logging.Info(sprintf "Launching %s: %O" name (DateTime.Now.ToString()))
        let window =
            try
                Some (new Window(config, name, root))
            with err -> Logging.Critical(name + " failed to launch", err); Console.ReadLine() |> ignore; None
        if (window.IsSome) then
            let mutable crashed = false
            use window = window.Value
            try
                window.Run()
                Logging.Info "Stopping"
            with err -> Logging.Critical("Fatal error in window thread", err); crashed <- true
            window.Close()
            if crashed then ignore(Console.ReadLine())