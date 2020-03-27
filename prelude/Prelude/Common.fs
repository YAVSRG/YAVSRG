module Prelude.Common

open System.Threading
open System.Threading.Tasks
open System.IO
open Newtonsoft.Json

(*
    Json I/O
*)

type Json() =
    static let s = JsonSerializerSettings() |> JsonSerializer.Create
    
    static member Load<'T> string : 'T = 
        use jr = new JsonTextReader(new StringReader(string))
        s.Deserialize<'T> jr

    static member Save<'T> (obj : 'T) : string = 
        use sw = new StringWriter()
        use jw = new JsonTextWriter(sw)
        s.Serialize(jw, obj)
        sw.ToString()

    static member LoadFile<'T> (path : string) : 'T =
        use jr = new JsonTextReader(new StreamReader(path))
        s.Deserialize<'T> jr
        
    static member SaveFile<'T> (obj : 'T) (path : string) : unit = 
        use sw = new StreamWriter(path)
        use jw = new JsonTextWriter(sw)
        s.Serialize(jw, obj)

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

Logging.Subscribe (fun (level, main, details) -> printf "[%A]: %s\n" level main)

(*
    Localisation
    (stub)
*)

let localise str : string = str

(*
    Background task management
*)

type LoggableTask = (string -> unit) -> (unit -> bool)
type ManagedTaskState = INACTIVE = 0 | ACTIVE = 1 | DONE = 2

type ManagedTask(name : string, t : LoggableTask, callback : bool -> unit, visible : bool) =
    let callback = callback
    let visible = visible
    let cts = new CancellationTokenSource()
    let mutable state = ManagedTaskState.INACTIVE
    let mutable status = "Pending"
    let mutable task : Task = new Task(fun () -> ())

    //this exists so that...
    member this.CreateTask =
        let action() =
            try
                status <- "Running"
                callback(t (fun s -> status <- s; Logging.Info (name + ": " + s) "") ())
                status <- "Complete"
            with
            | err ->
                Logging.Error ("Exception in " + name) (err.ToString())
                status <- "Failed"
            state <- ManagedTaskState.DONE
            cts.Dispose()
            //... I can have this line of code here. This member is used once to initialise task
            TaskManager.RemoveTask(this)
        new Task(action, cts.Token)

    member this.Name = name

    member this.Start =
        if state = ManagedTaskState.INACTIVE then
            task <- this.CreateTask;
            state <- ManagedTaskState.ACTIVE
            task.Start();
        else Logging.Warn ("Tried to start " + name + " but it has already been started") ""

    member this.Status = task.Status

    member this.Cancel =
        if state <> ManagedTaskState.ACTIVE then Logging.Warn ("Tried to cancel " + name + " but it is not running") ""
        else cts.Cancel(); state <- ManagedTaskState.DONE

    member this.Visible = visible

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

    //todo: consider changing .Add to .Subscribe if the need arises
    static member Subscribe = evt.Publish.Add

TaskManager.Subscribe (fun t -> Logging.Debug ("Task created: " + t.Name) "")

(*
    Random helper functions for things I think I will do a lot of and couldn't find a shorthand for
    Lots of these are likely to be temporary while I learn the language and discover the already existing shorthand
*)

let extractOption o =
    match o with
    | Some v -> v
    | None -> failwith "impossible"