namespace Prelude

open System
open System.IO
open SixLabors.ImageSharp
open System.Drawing
open System.Threading
open System.Threading.Tasks
open System.Collections.Generic
open Percyqaz.Json
open Percyqaz.Common

module Common =

    #if DEBUG
    let DEBUG_MODE = true
    #else
    let DEBUG_MODE = false
    #endif

    [<Measure>] type ms = Percyqaz.Common.ms
    [<Measure>] type beat
    [<Measure>] type minute
    type Time = Percyqaz.Common.Time
    let inline toTime (f: float) = float32 f * 1.0f<ms>

    module Time =
        let Abs (t: Time) = if t < 0.0f<ms> then -t else t

        let infinity = infinityf * 1.0f<ms>

    type SettingCodec<'T, 'Config>() =
        inherit Json.Codec<Setting<'T, 'Config>>()
        override this.To (ctx: Json.Context) =
            let cdc = ctx.GetCodec<'T>()
            fun o -> cdc.To o.Value
        override this.From (ctx: Json.Context) =
            let cdc = ctx.GetCodec<'T>()
            fun o json -> o.Value <- cdc.From o.Value json; o
        override this.Default (ctx: Json.Context) =
            let cdc = ctx.GetCodec<'T>()
            fun () ->
                let v = cdc.Default()
                { Set = ignore; Get = K v; Config = Unchecked.defaultof<_> }

    type ColorCodec() =
        inherit Json.Codec<Color>()
        override this.To (ctx: Json.Context) =
            let cdc = ctx.GetCodec<byte * byte * byte * byte>()
            fun col -> cdc.To (col.A, col.R, col.G, col.B)
        override this.From (ctx: Json.Context) =
            let cdc = ctx.GetCodec<byte * byte * byte * byte>()
            fun _ json -> 
                let (a, r, g, b) = cdc.FromDefault json
                Color.FromArgb(int a, int r, int g, int b)
        override this.Default (ctx: Json.Context) =
            fun () -> Color.White

    type ConcurrentDictionaryCodec<'K, 'V when 'K : equality>() =
        inherit Json.Codec<Collections.Concurrent.ConcurrentDictionary<'K, 'V>>()
        override this.To (ctx: Json.Context) =
            let cdc = ctx.GetCodec<Dictionary<'K, 'V>>()
            fun cd -> cdc.To (Dictionary cd)
        override this.From (ctx: Json.Context) =
            let cdc = ctx.GetCodec<Dictionary<'K, 'V>>()
            fun _ json -> Collections.Concurrent.ConcurrentDictionary(cdc.FromDefault json)
        override this.Default (ctx: Json.Context) =
            fun () -> Collections.Concurrent.ConcurrentDictionary()

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
                loadedPath <- Path.GetFullPath path
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
    Misc helpers (mostly data storage)
*)
    
    let JSON =
        Json(Json.Settings.Default)
            .WithDefaults()
            .WithCodec<SettingCodec<_,_>>()
            .WithCodec<ColorCodec>()
            .WithCodec<ConcurrentDictionaryCodec<_,_>>()

    type Bitmap = Image<PixelFormats.Rgba32>
    module Bitmap =
        let load (stream: Stream) : Bitmap = Bitmap.Load<PixelFormats.Rgba32> stream
    type Color = Drawing.Color


    let getDataPath name =
        let p = Path.Combine(Directory.GetCurrentDirectory(), name)
        Directory.CreateDirectory p |> ignore
        p

    let loadImportantJsonFile<'T> name path prompt =
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
                JSON.Default<'T>()
        else
            Logging.Info (sprintf "No %s file found, creating it." name)
            JSON.Default<'T>()

    let saveImportantJsonFile<'T> path (data: 'T) =
        let write = Path.ChangeExtension (path, ".new")
        let bak = Path.ChangeExtension (path, ".bak")
        let bak2 = Path.ChangeExtension (path, ".bak2")
        JSON.ToFile (write, true) data
        if File.Exists bak2 then File.Delete bak2
        if File.Exists bak then File.Move (bak, bak2)
        if File.Exists path then File.Move (path, bak)
        File.Move (write, path)