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
        let Abs(t: Time) = if t < 0.0f<ms> then -t else t

(*
    Settings - Store an (ideally immutably typed) value that can be get and set
    Aims to provide extension to add restrictions and a consistent format so that it is easy to auto-generate UI components that edit settings
    (Auto generation will be done with reflection)
*)

    [<AbstractClass>]
    type ISettable<'T>() =
        abstract member Value: 'T with get, set
        member this.Apply f = this.Value <- f this.Value
        override this.ToString() = this.Value.ToString()

    type WrappedSetting<'T, 'U>(setting: ISettable<'U>, set: 'T -> 'U, get: 'U -> 'T) =
        inherit ISettable<'T>()
        override this.Value with get() = get setting.Value and set newValue = setting.Value <- set newValue

    type Setting<'T>(value: 'T) =
        inherit ISettable<'T>()
        let mutable value = value
        override this.Value with get() = value and set newValue = value <- newValue
        static member Pickler: Json.Mapping.JsonPickler<Setting<'T>> =
            let tP = Json.Mapping.getPickler<'T>()
            Json.Mapping.mkPickler
                (fun (o: Setting<'T>) -> tP.Encode o.Value)
                (fun (o: Setting<'T>) json -> tP.Decode o.Value json |> JsonMapResult.map (fun v -> o.Value <- v; o))

    [<AbstractClass>]
    type NumSetting<'T when 'T : comparison>(value: 'T, min: 'T, max: 'T) =
        inherit Setting<'T>(value)
        abstract member ValuePercent: float32 with get, set
        override this.Value
            with set(newValue) = base.Value <- if newValue > max then max elif newValue < min then min else newValue
            and get() = base.Value
        member this.Min = min
        member this.Max = max
        override this.ToString() = sprintf "%s (%A - %A)" (base.ToString()) min max
        static member Pickler: Json.Mapping.JsonPickler<NumSetting<'T>> =
            let tP = Json.Mapping.getPickler<'T>()
            Json.Mapping.mkPickler
                (fun (o: NumSetting<'T>) -> tP.Encode o.Value)
                (fun (o: NumSetting<'T>) json -> tP.Decode o.Value json |> JsonMapResult.map (fun v -> o.Value <- v; o))

    type IntSetting(value: int, min: int, max: int) =
        inherit NumSetting<int>(value, min, max)
        override this.ValuePercent
            with set(pc: float32) = this.Value <- min + ((float32 (max - min) * pc) |> float |> Math.Round |> int)
            and get() = float32 (this.Value - min) / float32 (max - min)
        static member Pickler = Setting<int>.Pickler

    type FloatSetting(value: float, min: float, max: float) =
        inherit NumSetting<float>(value, min, max)
        override this.ValuePercent
            with set(pc: float32) = this.Value <- min + (max - min) * float pc
            and get() = (this.Value - min) / (max - min) |> float32
        override this.Value
            with set(newValue: float) = base.Value <- Math.Round(newValue, 2)
            and get() = base.Value
        static member Pickler = Setting<float>.Pickler

    type StringSetting(value: string, allowSpecialChar: bool) =
        inherit Setting<string>(value)
        override this.Value
            with set(newValue) = base.Value <- if allowSpecialChar then newValue else Regex("[^a-zA-Z0-9_-]").Replace(newValue, "")
            and get() = base.Value
        static member Pickler = Setting<string>.Pickler

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
        static member Log level main details = agent.Post(level, main, details.ToString())

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
        member this.Zero () = this.Return ()
    let profile name = new ProfilingBuilder(name)

(*
    Localisation
*)

    module Localisation =
        let private mapping = new Dictionary<string, string>()
        let mutable private loadedPath = ""

        let loadFile path =
            let path = Path.Combine("Locale", path)
            try
                let lines = File.ReadAllLines path
                Array.iter(
                    fun (l: string) ->
                        let s: string[] = l.Split([|'='|], 2)
                        mapping.Add (s.[0], s.[1])
                    ) lines
                loadedPath <- path
            with err -> Logging.Error("Failed to load localisation file: " + path, err)

        let localise str : string =
            if mapping.ContainsKey str then mapping.[str]
            else
                mapping.Add (str, str)
                if loadedPath <> "" then File.AppendAllText(loadedPath, "\n"+str+"="+str)
                str

        let localiseWith xs str =
            let mutable s = localise str
            List.iteri (fun i x -> s <- s.Replace("%"+i.ToString(), x)) xs
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

(*
    Misc helper functions (mostly data storage)
*)

    let getDataPath name =
        let p = Path.Combine(Directory.GetCurrentDirectory(), name)
        Directory.CreateDirectory p |> ignore
        p

    let loadImportantJsonFile<'T> name path (defaultData: 'T) prompt =
        if File.Exists path then
            let p = Path.ChangeExtension(path, ".bak")
            if File.Exists p then File.Copy(p, Path.ChangeExtension(path, ".bak2"), true)
            File.Copy(path, p, true)
            try
                Json.fromFile path |> JsonResult.value
            with err ->
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

    do
        let tP = Json.Mapping.getPickler<byte * byte * byte * byte>()
        Json.Mapping.Rules.addTypeRuleWithDefault
            (fun (c: Color) -> tP.Encode (c.A, c.R, c.G, c.B))
            (fun (c: Color) json -> tP.Decode (c.A, c.R, c.G, c.B) json |> JsonMapResult.map (fun (a, r, g, b) -> Color.FromArgb(int a, int r, int g, int b)))
            Color.White
        Json.Mapping.Rules.addPicklerRule()