open System
open System.Threading
open System.Diagnostics
open System.IO
open Prelude.Common
open Interlude

[<EntryPoint>]
let main argv =
    let m = new Mutex(true, "Interlude")

    let crashSplash = Utils.randomSplash("CrashSplashes.txt") >> (fun s -> Logging.Critical s)

    //Check if interlude is already running
    if m.WaitOne(TimeSpan.Zero, true) then
        Process.GetCurrentProcess().PriorityClass <- ProcessPriorityClass.High
        //Init logging
        use logfile = File.Open("log.txt", FileMode.Append)
        use sw = new StreamWriter(logfile)
        Logging.Subscribe
            ( fun (level, main, details) ->
                if details = "" then sprintf "[%A] %s" level main else sprintf "[%A] %s\n%s" level main details
                |> sw.WriteLine )

        Logging.Info("Launching " + Utils.version + ", " + DateTime.Now.ToString())
        let game =
            try
                Options.load()
                Audio.init(Options.config.AudioDevice.Value)
                Some (new Game(Options.config))
            with err -> Logging.Critical("Game failed to launch", err); crashSplash(); Console.ReadLine() |> ignore; None
        if (game.IsSome) then
            let mutable crashed = false
            let game = game.Value
            try
                game.Run()
                Logging.Info "Exiting game"
            with err -> Logging.Critical("Game crashed", err); crashSplash(); crashed <- true
            game.Close()
            Options.save()
            game.Dispose()
            if crashed then ignore(Console.ReadLine())
        Logging.Wait()
        m.ReleaseMutex()
    else
        //todo: code that sends data to the running process to reappear if hidden
        ()
    0
