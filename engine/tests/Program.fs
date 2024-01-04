open System
open System.IO
open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.Tests

[<EntryPoint>]
let main argv =
    use logfile = File.Open("log.txt", FileMode.Append)
    use sw = new StreamWriter(logfile)

    Logging.Subscribe(fun (level, main, details) ->
        if details = "" then
            sprintf "[%A] %s" level main
        else
            sprintf "[%A] %s\n%s" level main details
        |> sw.WriteLine
    )

    Logging.Info("Launching flux-tests, " + DateTime.Now.ToString())

    let window =
        try
            Some(
                new Window(
                    { Config.Default with
                        WindowMode = Setting.simple WindowType.Windowed
                        FrameLimit = Setting.simple FrameLimit.Unlimited
                    },
                    "Percyqaz.Flux.Tests",
                    TopLevel()
                )
            )
        with err ->
            Logging.Critical("Flux failed to launch", err)
            Console.ReadLine() |> ignore
            None

    if (window.IsSome) then
        let mutable crashed = false
        use window = window.Value

        try
            window.Run()
            Logging.Info "Exiting game"
        with err ->
            Logging.Critical("Flux crashed", err)
            crashed <- true

        window.Close()

        if crashed then
            ignore (Console.ReadLine())

    Logging.Wait()

    0
