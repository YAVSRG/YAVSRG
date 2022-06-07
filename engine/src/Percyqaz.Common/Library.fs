namespace Percyqaz.Common

open System
open System.Diagnostics
open System.Text.RegularExpressions
open System.Threading

[<AutoOpen>]
module Combinators =
    
    let K x _ = x

[<Measure>]
type ms
type Time = float32<ms>

type NYI = unit

type Setting<'T, 'Config> =
    {
        Set: 'T -> unit
        Get: unit -> 'T
        Config: 'Config
    }
    member this.Value with get() = this.Get() and set(v) = this.Set(v)
    override this.ToString() = sprintf "<%O, %A>" this.Value this.Config

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

type LoggingLevel = DEBUG = 0 | INFO = 1 | WARNING = 2 | ERROR = 3 | CRITICAL = 4
type LoggingEvent = LoggingLevel * string * string

[<AutoOpen>]
module Logging =

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

[<AutoOpen>]
module Profiling =

    type ProfilingBuilder(name) =
        let sw = Stopwatch.StartNew()
        member this.Return x =
            Logging.Debug (sprintf "%s: took %.0fms" name sw.Elapsed.TotalMilliseconds)
            x
        member this.Zero() = this.Return()

    let profile name = new ProfilingBuilder(name)

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