namespace Prelude

open System
open System.IO
open System.Diagnostics
open SixLabors.ImageSharp
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

        let infinity = infinityf * 1.0f<ms>

(*
    Settings - Store an (ideally immutable) value that can be get and set, basically like a reference cell
    Extensible to add restrictions and triggers when settings are changed
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

        let map (after_get: 'T -> 'U) (before_set: 'U -> 'T) (setting: Setting<'T, 'Config>) =
            {
                Set = before_set >> setting.Set
                Get = after_get << setting.Get
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
            let regex = Regex("[^\sa-zA-Z0-9_-]")
            map id (fun s -> regex.Replace(s, "")) setting

        let inline bounded x min max =
            simple x
            |> bound min max

        let percent x = bounded x 0.0 1.0 |> round 2
        let percentf x = bounded x 0.0f 1.0f |> roundf 2

        let rate x = bounded x 0.5f 2.0f |> roundf 2
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

        static member Wait() =
            while agent.CurrentQueueLength > 0 do
                Thread.Sleep(200)

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
                        mapping.Add (s.[0], s.[1].Replace("\\n","\n"))
                    ) lines
                loadedPath <- path
            with err -> Logging.Error ("Failed to load localisation file: " + path, err)

        let localise str : string =
            if mapping.ContainsKey str then mapping.[str]
            else
                mapping.Add (str, str)
                if DEBUG_MODE && loadedPath <> "" then File.AppendAllText (loadedPath, "\n"+str+"="+str)
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
            member this.Visible = visible
            member this.Info = info

            member this.Wait() = Async.AwaitTask task |> Async.RunSynchronously
            member this.Cancel() =
                try
                    cts.Cancel false
                    Logging.Debug(sprintf "Task <%s> cancelled" name)
                with _ -> ()

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
                
(*
    Async tools v2
*)
    
    module Async =
    
        /// Allows you to request some asynchronous work to be done, with a callback when it completes
        /// If you use a higher level of concurrency, Results may not come back in the order they were requested
        [<AbstractClass>]
        type ManyWorker<'Request, 'Reply>() as this =
            let worker = 
                MailboxProcessor<'Request>.Start
                    ( fun box -> 
                        let rec loop () = async {
                            let! request = box.Receive()
                            let res = this.Handle request
                            this.Callback(request, res)
                            return! loop ()
                        }
                        loop ()
                    )
        
            abstract member Handle: 'Request -> 'Reply
        
            abstract member Callback: 'Request * 'Reply -> unit
        
            member this.Request(req: 'Request) : unit =
                worker.Post req

        type Job<'T> = int * 'T

        /// Allows you to request some asynchronous work to be done
        ///  If another job is requested before the first completes, the result of the outdated job is swallowed
        /// This allows easy reasoning about background jobs and how their results join with the main update loop
        [<AbstractClass>]
        type SingletonWorker<'Request, 'Reply>() as this =
            let mutable job_number = 0
            let lockObj = obj()

            let worker = 
                MailboxProcessor<Job<'Request>>.Start
                    ( fun box -> 
                        let rec loop () = async {
                            let! id, request = box.Receive()
                            let processed = this.Handle request
                            lock lockObj ( fun () -> if id = job_number then this.Callback(processed) )
                            return! loop ()
                        }
                        loop ()
                    )

            abstract member Handle: 'Request -> 'Reply

            abstract member Callback: 'Reply -> unit

            member this.Request(req: 'Request) : unit =
                lock lockObj ( fun () -> 
                    job_number <- job_number + 1
                    worker.Post(job_number, req)
                )
                
        /// Allows you to request some asynchronous work to be done
        /// This version processes a sequence of items and runs a callback as each is completed
        ///  If another job is requested before the first completes, the results of the outdated job are swallowed
        /// This allows easy reasoning about background jobs and how their results join with the main update loop
        [<AbstractClass>]
        type SingletonWorkerSeq<'Request, 'Reply>() as this =
            let mutable job_number = 0
            let lockObj = obj()

            let worker = 
                MailboxProcessor<Job<'Request>>.Start
                    ( fun box -> 
                        let rec loop () = async {
                            let! id, request = box.Receive()
                            for processed in this.Handle request do
                                lock lockObj ( fun () -> if id = job_number then this.Callback(processed) )
                            lock lockObj ( fun () -> if id = job_number then this.JobCompleted(request) )
                            return! loop ()
                        }
                        loop ()
                    )

            abstract member Handle: 'Request -> 'Reply seq

            abstract member Callback: 'Reply -> unit

            abstract member JobCompleted: 'Request -> unit

            member this.Request(req: 'Request) : unit =
                lock lockObj ( fun () -> job_number <- job_number + 1 )
                worker.Post(job_number, req)

(*
    Misc helpers (mostly data storage)
*)
    
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

    type Bitmap = Image<PixelFormats.Rgba32>
    module Bitmap =
        let load (stream: Stream) : Bitmap = Bitmap.Load<PixelFormats.Rgba32> stream
    type Color = Drawing.Color


    let getDataPath name =
        let p = Path.Combine(Directory.GetCurrentDirectory(), name)
        Directory.CreateDirectory p |> ignore
        p

    let loadImportantJsonFile<'T> name path (defaultData: 'T) prompt =
        if File.Exists path then
            match JSON.FromFile path with
            | Ok data -> data
            | Error err ->
                Logging.Critical (sprintf "Could not load %s! Maybe it is corrupt?" (Path.GetFileName path), err)
                if prompt then
                    Console.WriteLine "If you would like to launch anyway (WILL WIPE THIS DATA!!), press ENTER."
                    Console.WriteLine "If you would like to try and fix the problem youself, CLOSE THIS WINDOW NOW."
                    Console.ReadLine() |> ignore
                    Logging.Critical "User has chosen to launch game with default data."
                defaultData
        else
            Logging.Info (sprintf "No %s file found, creating it." name)
            defaultData

    let saveImportantJsonFile<'T> path (data: 'T) =
        let write = Path.ChangeExtension (path, ".new")
        let bak = Path.ChangeExtension (path, ".bak")
        let bak2 = Path.ChangeExtension (path, ".bak2")
        JSON.ToFile (write, true) data
        if File.Exists bak2 then File.Delete bak2
        if File.Exists bak then File.Move (bak, bak2)
        if File.Exists path then File.Move (path, bak)
        File.Move (write, path)