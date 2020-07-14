namespace Prelude

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open System.Collections.Generic
open System.Text.RegularExpressions

module Common =

    [<Measure>] type ms
    [<Measure>] type beat
    [<Measure>] type minute
    type Time = float32<ms>
    let toTime (f: float) = float32 f * 1.0f<ms>

    module Time =
        let Abs(t: Time) = if t < 0.0f<ms> then -t else t

(*
    Settings - Store an (ideally immutable type) value that can be get and set
    Aims to provide extension to add restrictions and a consistent format so that it is easy to auto-generate UI components that edit settings
    (Auto generation will be done with reflection)
*)
    [<AbstractClass>]
    type ISettable<'T>() =
        abstract member Set: 'T -> unit
        abstract member Get: unit -> 'T

    type Setting<'T>(value : 'T) =
        inherit ISettable<'T>()
        let mutable value = value
        override this.Set(newValue) = value <- newValue
        override this.Get() = value
        override this.ToString() = value.ToString()

    type WrappedSetting<'T, 'U>(setting: Setting<'U>, set: 'T -> 'U, get: 'U -> 'T) =
        inherit ISettable<'T>()
        override this.Set(newValue) = setting.Set(set(newValue))
        override this.Get() = get(setting.Get())

    [<AbstractClass>]
    type NumSetting<'T when 'T : comparison>(value : 'T, min : 'T, max : 'T) =
        inherit Setting<'T>(value)
        abstract member SetPercent: float32 -> unit
        abstract member GetPercent: unit -> float32
        override this.Set(newValue) = base.Set(if newValue > max then max elif newValue < min then min else newValue)
        member this.Min = min
        member this.Max = max
        override this.ToString() = sprintf "%s (%A - %A)" (base.ToString()) min max

    type IntSetting(value: int, min: int, max: int) = 
        inherit NumSetting<int>(value, min, max)
        override this.SetPercent(pc: float32) = this.Set(min + ((float32 (max - min) * pc) |> float |> Math.Round |> int))
        override this.GetPercent() = float32 (this.Get() - min) / float32 (max - min)

    type FloatSetting(value: float, min: float, max: float) = 
        inherit NumSetting<float>(value, min, max)
        override this.SetPercent(pc: float32) = this.Set(min + (max - min) * float pc)
        override this.GetPercent() = (this.Get() - min) / (max - min) |> float32
        override this.Set(newValue: float) = base.Set(Math.Round(newValue, 2))

    type StringSetting(value: string, allowSpecialChar: bool) =
        inherit Setting<string>(value)
        override this.Set(newValue) = base.Set(if allowSpecialChar then newValue else Regex("[^a-zA-Z0-9_-]").Replace(newValue, ""))

(*
    Logging
*)

    type LoggingLevel = DEBUG = 0 | INFO = 1 | WARNING = 2 | ERROR = 3 | CRITICAL = 4
    type LoggingEventArgs = LoggingLevel * string * string

    type Logging() =
        static let evt = new Event<LoggingEventArgs>()

        static member Subscribe f = evt.Publish.Add f
        static member Log level main details = evt.Trigger((level, main, details))
        static member Info = Logging.Log LoggingLevel.INFO
        static member Warn = Logging.Log LoggingLevel.WARNING
        static member Error = Logging.Log LoggingLevel.ERROR
        static member Debug = Logging.Log LoggingLevel.DEBUG
        static member Critical = Logging.Log LoggingLevel.CRITICAL

    Logging.Subscribe (fun (level, main, details) -> Console.WriteLine(sprintf "[%A]: %s" level main))

(*
    Localisation
*)
    module Localisation =
        let private mapping = new Dictionary<string, string>()
        let mutable private loadedPath = ""

        let loadFile path =
            let path = Path.Combine("Locale", path)
            try
                let lines = File.ReadAllLines(path)
                Array.iter(
                    fun (l: string) ->
                        let s: string[] = l.Split([|'='|], 2)
                        mapping.Add(s.[0], s.[1])) lines
                loadedPath <- path
            with
            | err -> Logging.Error("Failed to load localisation file: " + path)(err.ToString())

        let localise str : string =
            if mapping.ContainsKey(str) then mapping.[str]
            else 
                mapping.Add(str, str)
                if loadedPath <> "" then File.AppendAllText(loadedPath, "\n"+str+"="+str)
                str

(*
    Background task management
*)

    type LoggableTask = (string -> unit) -> bool
    type ManagedTaskState = INACTIVE = 0 | ACTIVE = 1 | DONE = 2

    type ManagedTask(name : string, t : LoggableTask, callback : bool -> unit, visible : bool) =
        let callback = callback
        let visible = visible
        let cts = new CancellationTokenSource()
        let mutable state = ManagedTaskState.INACTIVE
        let mutable info = "Pending"
        let mutable task : Task = new Task(fun () -> ())

        //this exists so that...
        member this.CreateTask =
            let action() =
                try
                    info <- "Running"
                    callback(t (fun s -> info <- s; if not visible then Logging.Debug (name + ": " + s) ""))
                    info <- "Complete"
                with
                | err ->
                    Logging.Error ("Exception in " + name) (err.ToString())
                    info <- "Failed"
                state <- ManagedTaskState.DONE
                cts.Dispose()
                //... I can have this line of code here. This member is used once to initialise task
                TaskManager.RemoveTask(this)
            new Task(action, cts.Token)

        member this.Name = name
        member this.Status = task.Status
        member this.Info = info
        member this.Visible = visible

        member this.Start =
            if state = ManagedTaskState.INACTIVE then
                task <- this.CreateTask;
                state <- ManagedTaskState.ACTIVE
                task.Start();
            else Logging.Warn ("Tried to start " + name + " but it has already been started") ""

        member this.Cancel =
            if state <> ManagedTaskState.ACTIVE then Logging.Warn ("Tried to cancel " + name + " but it is not running") ""
            else cts.Cancel(); state <- ManagedTaskState.DONE

    and TaskManager() =
        static let evt = new Event<ManagedTask>()

        static let tasks = new System.Collections.Generic.List<ManagedTask>()

        static member CancelAll =
            lock (tasks) (fun () ->
                tasks.ForEach(fun x -> x.Cancel))

        static member HasTaskRunning =
            lock (tasks) (fun () ->
                not (tasks.TrueForAll(fun x -> x.Status <> TaskStatus.Running)))

        static member AddTask(t : ManagedTask) = 
            lock (tasks) (fun () ->
                tasks.Add(t); t.Start)
            evt.Trigger(t)
        static member AddTask(name, t, callback, visible) = TaskManager.AddTask(ManagedTask(name,t,callback,visible))

        static member RemoveTask(t : ManagedTask) = lock (tasks) (fun () -> tasks.Remove(t) |> ignore)

        static member Subscribe = evt.Publish.Add

        static member Wait = while TaskManager.HasTaskRunning do Thread.Sleep(1000)

    TaskManager.Subscribe (fun t -> Logging.Debug ("Task created: " + t.Name) "")

(*
    Random helper functions for things I think I will do a lot of and couldn't find a shorthand for
    Lots of these are likely to be temporary while I learn the language and discover the already existing shorthand
*)

    let getDataPath name =
        let p = Path.Combine(Directory.GetCurrentDirectory(), name)
        Directory.CreateDirectory(p) |> ignore
        p