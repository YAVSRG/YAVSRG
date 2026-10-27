namespace Prelude.Data.Imports

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

    let progress_ev = Event<TaskProgress>()

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

    member this.SubscribeProgress(callback: TaskProgress -> unit) : TaskContext =
        progress_ev.Publish.Add(callback)
        this

    member this.SubscribeDownloadProgress(callback: float32 -> unit) : TaskContext =
        this.SubscribeProgress(
            function
            | Downloading pc -> callback pc
            | _ -> ()
        )

    member this.LogProgress() : TaskContext =
        let mutable download_step = -1

        this.SubscribeProgress(
            function
            | Generic status -> Logging.Warn "%s: %s" label status
            | Downloading p ->
                let t = p / 0.1f |> floor |> int

                if t > download_step then
                    download_step <- t

                    Logging.Info
                        "%s: [%s%s] %.0f%%"
                        label
                        (String.replicate download_step "#")
                        (String.replicate (max 0 (10 - download_step)) "-")
                        (p * 100.0f)
            | Processing(i, count) -> Logging.Info "%s: %i / %i" label i count
            | Nested(nlabel, i, count, inner) -> Logging.Info "%s: %s %i/%i [%A]" label nlabel i count inner
        )
