// Learn more about F# at http://fsharp.org

open System
open System.Threading
open System.Diagnostics
open System.IO
open Prelude.Common
open Interlude
open Interlude.Options

[<EntryPoint>]
let main argv =
    let m = new Mutex(true, "Interlude")

    //Check if interlude is already running (true if not already running)
    if m.WaitOne(TimeSpan.Zero, true) then

        Process.GetCurrentProcess().PriorityClass <- ProcessPriorityClass.High

        //Init logging
        use logfile = File.Open("log.txt", FileMode.Append)
        use sw = new StreamWriter(logfile)
        Logging.Subscribe
            (fun (level, main, details) ->
                lock(sw) (fun _ ->
                if details = "" then sprintf "[%A] %s" level main else sprintf "[%A] %s\n%s" level main details
                |> sw.WriteLine))

        Logging.Info("Launching Interlude " + Game.version + ", " + DateTime.Now.ToString()) ""
        let game =
            try
                Options.load()
                Some (new Game())
            with
            | err -> Logging.Critical "Game failed to launch" (err.ToString()); None
        if (game.IsSome) then
            let game = game.Value
            try
                game.Run(120.0)
            with
            | err ->
                Logging.Critical "Game crashed" (err.ToString())
            game.Exit()
            Options.save()
            game.Dispose()
        m.ReleaseMutex()
    else
        //todo: code that sends data to the running process to reappear if hidden
        ()
    0
