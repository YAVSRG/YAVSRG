namespace Prelude.Data

open System.Runtime.CompilerServices
open Percyqaz.Common
open Prelude

[<AbstractClass>]
type ITaskContext() =
    abstract member Progress: TaskProgress -> unit

    member this.Subtask(name: string, count: int, total: int) : ITaskContext =
        { new ITaskContext() with
            override _.Progress(p: TaskProgress) : unit =
                this.Progress(Nested(name, count, total, p))
        }

    member this.Status(message: string) : unit = this.Progress(Generic(message))
    member this.DownloadProgress(percentage: float32) : unit = this.Progress(Downloading(percentage))
    member this.Progress(count: int, total: int) : unit = this.Progress(Processing(count, total))

    // temp
    member this.set_Progress p = this.Progress(p)

type TaskContext(label: string) =
    inherit ITaskContext()

    let mutable completed_at = ValueNone
    let mutable status = InProgress(Generic(%"import.waiting"))

    do Logging.Debug "Queuing task '%s'" label

    member this.Status
        with get (): TaskStatus = lock this (fun () -> status)
        and private set (v: TaskStatus) = lock this (fun () -> status <- v)

    override this.Progress(p: TaskProgress) : unit =
        match status with
        | InProgress _ -> this.Status <- InProgress(p)
        | _ -> ()

    member this.Faulted() : unit =
        match status with
        | InProgress _ -> this.Status <- Faulted
        | _ -> ()

    member this.Completed() : unit =
        match status with
        | InProgress _ ->
            this.Status <- Completed
            lock this (fun () -> completed_at <- ValueSome(Timestamp.now()))
        | _ -> ()

    member val StartedAt: int64 = Timestamp.now()
    member this.CompletedAt: int64 voption = lock this (fun () -> completed_at)
    member this.Label: string = label
