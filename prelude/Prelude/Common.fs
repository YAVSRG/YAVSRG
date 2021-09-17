namespace Prelude

open System
open System.IO
open System.Diagnostics
open System.Drawing
open System.Threading
open System.Threading.Tasks
open System.Collections.Generic
open System.Text.RegularExpressions
open Percyqaz.Json

module Common =

    #if DEBUG
    let DEBUG_MODE = true
    #else
    let DEBUG_MODE = false
    #endif

    [<Measure>] type ms
    [<Measure>] type beat
    [<Measure>] type minute
    type Time = float32<ms>
    let inline toTime (f: float) = float32 f * 1.0f<ms>

    module Time =
        let Abs (t: Time) = if t < 0.0f<ms> then -t else t

(*
    Settings - Store an (ideally immutably typed) value that can be get and set
    Aims to provide extension to add restrictions and a consistent format so that it is easy to auto-generate UI components that edit settings
    (Auto generation will be done with reflection)
*)

    type Setting<'T, 'Config> =
        {
            Set: 'T -> unit
            Get: unit -> 'T
            Config: 'Config
        }
        member this.Value with get() = this.Get() and set(v) = this.Set(v)
        override this.ToString() = sprintf "<%O, %A>" this.Value this.Config
        static member JsonCodec(cache, settings, rules) : Json.Mapping.JsonCodec<Setting<'T, 'Config>> =
            let tP = Json.Mapping.getCodec<'T>(cache, settings, rules)
            {
                Encode = fun o -> tP.Encode o.Value
                Decode = fun o json -> o.Value <- tP.Decode o.Value json; o
                Default = fun () -> failwith "Default instance should be provided for settings"
            }

    type Setting<'T> = Setting<'T, unit>


    module Setting =

        type Bounds<'T> = Bounds of min: 'T * max: 'T
        type Bounded<'T> = Setting<'T, Bounds<'T>>
        
        let simple (x: 'T) =
            let mutable x = x
            {
                Set = fun v -> x <- v
                Get = fun () -> x
                Config = ()
            }

        let make (set: 'T -> unit) (get: unit -> 'T) =
            {
                Set = set
                Get = get
                Config = ()
            }

        let map (f: 'T -> 'U) (g: 'U -> 'T) (setting: Setting<'T, 'Config>) =
            {
                Set = g >> setting.Set
                Get = f << setting.Get
                Config = setting.Config
            }

        let iter (f: 'T -> unit) (setting: Setting<'T, 'Config>) = f setting.Value
        let app (f: 'T -> 'T) (setting: Setting<'T, 'Config>) = setting.Value <- f setting.Value
        let trigger (action: 'T -> unit) (setting: Setting<'T, 'Config>) = { setting with Set = fun x -> setting.Set x; action x }

        let inline bound (min: 'T) (max: 'T) (setting: Setting<'T, 'Config>) =
            if min > max then invalidArg (nameof min) "min cannot be more than max"
            {
                Set =
                    fun v ->
                        if v < min then setting.Set min
                        elif v > max then setting.Set max
                        else setting.Set v
                Get = setting.Get
                Config = Bounds (min, max)
            }

        let inline round (dp: int) (setting: Setting<float, 'Config>) =
            { setting with
                Set = fun v -> setting.Set (Math.Round (v, dp))
            }

        let inline roundf (dp: int) (setting: Setting<float32, 'Config>) =
            { setting with
                Set = fun v -> setting.Set (MathF.Round (v, dp))
            }

        let alphaNum (setting: Setting<string, 'Config>) =
            let regex = Regex("[^a-zA-Z0-9_-]")
            map id (fun s -> regex.Replace(s, "")) setting

        let inline bounded x min max =
            simple x
            |> bound min max

        let percent x = bounded x 0.0 1.0 |> round 2

(*
    Logging
*)

    type LoggingLevel = DEBUG = 0 | INFO = 1 | WARNING = 2 | ERROR = 3 | CRITICAL = 4
    type LoggingEvent = LoggingLevel * string * string

    type Logging() =
        static let evt = new Event<LoggingEvent>()

        static let agent = new MailboxProcessor<LoggingEvent>(fun box -> async { while (true) do let! e = box.Receive() in evt.Trigger e })
        static do agent.Start()

        static member Subscribe f = evt.Publish.Add f
        static member Log level main details = agent.Post (level, main, details.ToString())

        static member Info (s, err) = Logging.Log LoggingLevel.INFO s err
        static member Warn (s, err) = Logging.Log LoggingLevel.WARNING s err
        static member Error (s, err) = Logging.Log LoggingLevel.ERROR s err
        static member Debug (s, err) = Logging.Log LoggingLevel.DEBUG s err
        static member Critical (s, err) = Logging.Log LoggingLevel.CRITICAL s err

        static member Info s = Logging.Log LoggingLevel.INFO s ""
        static member Warn s = Logging.Log LoggingLevel.WARNING s ""
        static member Error s = Logging.Log LoggingLevel.ERROR s ""
        static member Debug s = Logging.Log LoggingLevel.DEBUG s ""
        static member Critical s = Logging.Log LoggingLevel.CRITICAL s ""

    Logging.Subscribe (fun (level, main, details) -> printfn "[%A]: %s" level main; if level = LoggingLevel.CRITICAL then printfn " .. %s" details)
    
    (* Profiling tool *)
    type ProfilingBuilder(name) =
        let sw = Stopwatch.StartNew()
        member this.Return x =
            Logging.Debug (sprintf "%s: took %.0fms" name sw.Elapsed.TotalMilliseconds)
            x
        member this.Zero() = this.Return()
    let profile name = new ProfilingBuilder(name)

(*
    Localisation
*)

    module Localisation =
        let private mapping = new Dictionary<string, string>()
        let mutable private loadedPath = ""

        let loadFile path =
            let path = Path.Combine ("Locale", path)
            try
                let lines = File.ReadAllLines path
                Array.iter(
                    fun (l: string) ->
                        let s: string[] = l.Split ([|'='|], 2)
                        mapping.Add (s.[0], s.[1])
                    ) lines
                loadedPath <- path
            with err -> Logging.Error ("Failed to load localisation file: " + path, err)

        let localise str : string =
            if mapping.ContainsKey str then mapping.[str]
            else
                mapping.Add (str, str)
                if loadedPath <> "" then File.AppendAllText (loadedPath, "\n"+str+"="+str)
                str

        let localiseWith xs str =
            let mutable s = localise str
            List.iteri (fun i x -> s <- s.Replace ("%" + i.ToString(), x)) xs
            s

(*
    Background task management
*)

    type StatusTask = (string -> unit) -> Async<bool>
    type TaskFlags = NONE = 0 | HIDDEN = 1 | LONGRUNNING = 2

    module BackgroundTask =
        //Some race conditions exist but the data is only used by GUI code (and so only needs informal correctness)
        type ManagedTask (name: string, t: StatusTask, options: TaskFlags, removeTask) as this =
            let cts = new CancellationTokenSource()
            let mutable info = ""
            let visible = options &&& TaskFlags.HIDDEN <> TaskFlags.HIDDEN

            let task =
                Async.StartAsTask(
                    async {
                        try
                            if visible then Logging.Debug (sprintf "Task <%s> started" name)
                            info <- "Running"
                            let! outcome = t (fun s -> info <- s; if not visible then Logging.Debug (sprintf "[%s] %s" name s))
                            if visible then Logging.Debug (sprintf "Task <%s> complete (%A)" name outcome)
                            info <- "Complete"
                        with err ->
                            Logging.Error (sprintf "Exception in task '%s'" name, err)
                            info <- sprintf "Failed: %O" <| err.GetType()
                        removeTask this
                        cts.Dispose()
                        },
                    ((*if options &&& TaskFlags.LONGRUNNING = TaskFlags.LONGRUNNING then TaskCreationOptions.LongRunning else*) TaskCreationOptions.None),
                    cts.Token)

            member this.Name = name
            member this.Status = task.Status
            member this.Cancel() =
                try
                    cts.Cancel false
                    Logging.Debug(sprintf "Task <%s> cancelled" name)
                with _ -> ()
            member this.Visible = visible
            member this.Info = info

        let private evt = new Event<ManagedTask>()
        let Subscribe f = evt.Publish.Add f

        let TaskList = ResizeArray<ManagedTask>()
        let private removeTask = fun mt -> if lock TaskList (fun () -> TaskList.Remove mt) <> true then Logging.Debug(sprintf "Tried to remove a task that isn't there: %s" mt.Name)

        let Callback (f: bool -> unit) (proc: StatusTask) : StatusTask = fun (l: string -> unit) -> async { let! v = proc l in f v; return v }
        let rec Chain (procs: StatusTask list) : StatusTask =
            match procs with
            | [] -> failwith "should not be used on empty list"
            | x :: [] -> x
            | x :: xs ->
                let rest = Chain xs
                fun (l: string -> unit) ->
                    async { let! b = x l in if b then return! rest l else return false }

        let Create (options: TaskFlags) (name: string) (t: StatusTask) =
            let mt = new ManagedTask(name, t, options, removeTask)
            lock TaskList (fun () -> TaskList.Add mt)
            evt.Trigger mt
            mt

        let future<'T> (name: string) (callback: 'T -> unit) =
            let mutable id = 0
            let starter i cons: StatusTask =
                fun output -> async {
                    let v = cons()
                    if id = i then callback v; return true
                    else return false
                }
            fun (cons: unit -> 'T) ->
                id <- id + 1
                ignore <| Create TaskFlags.HIDDEN name (starter id cons)

        let futureSeq<'T> (name: string) (callback: 'T -> unit) =
            let mutable id = 0
            let starter i cons: StatusTask =
                fun output -> async {
                    for v in cons() do
                        if id = i then callback v
                    return id = i
                }
            fun (cons: unit -> 'T seq) ->
                id <- id + 1
                ignore <| Create TaskFlags.HIDDEN name (starter id cons)
    
    let JSON =
        let j = new JsonEncoder()
            
        fun (cache, settings, rules) -> 
            Json.Mapping.getCodec<byte * byte * byte * byte>(cache, settings, rules)
            |> Json.Mapping.Codec.map
                (fun (c: Color) -> (c.A, c.R, c.G, c.B))
                (fun (a, r, g, b) -> Color.FromArgb(int a, int r, int g, int b))
        |> Json.Mapping.Rules.typeRule<Color>
        |> j.AddRule

        j

(*
    Misc helper functions (mostly data storage)
*)

    let getDataPath name =
        let p = Path.Combine(Directory.GetCurrentDirectory(), name)
        Directory.CreateDirectory p |> ignore
        p

    let loadImportantJsonFile<'T> name path (defaultData: 'T) prompt =
        if File.Exists path then
            let p = Path.ChangeExtension (path, ".bak")
            if File.Exists p then File.Copy (p, Path.ChangeExtension (path, ".bak2"), true)
            File.Copy (path, p, true)
            match JSON.FromFile path with
            | Ok data -> data
            | Error err ->
                Logging.Critical (sprintf "Could not load %s! Maybe it is corrupt?" (Path.GetFileName path), err)
                if prompt then
                    Console.WriteLine "If you would like to launch anyway, press ENTER."
                    Console.WriteLine "If you would like to try and fix the problem youself, CLOSE THIS WINDOW."
                    Console.ReadLine() |> ignore
                    Logging.Critical "User has chosen to launch game with default data."
                defaultData
        else
            Logging.Info (sprintf "No %s file found, creating it." name)
            defaultData