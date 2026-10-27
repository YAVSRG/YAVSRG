namespace Prelude.Data

open System.Runtime.CompilerServices
open Percyqaz.Common

[<AbstractClass>]
type BackgroundTask<'T>() =
    abstract member Run: ITaskContext -> Async<'T>

    member this.Track(ctx: TaskContext) : Async<Result<'T, string>> =
        async {
            match! Async.Catch(this.Run(ctx)) with
            | Choice1Of2 result ->
                ctx.Completed()
                return Ok(result)
            | Choice2Of2 exn ->
                ctx.Faulted()
                Logging.Error "Unexpected exception during '%s': %O" ctx.Label exn
                return Error(exn.Message)
        }

type AsyncExtensions =

    [<Extension>]
    static member WithCallback(task: Async<Result<'T, _>>, callback: 'T -> unit) : Async<unit> =
        async {
            match! task with
            | Ok result -> callback(result)
            | Error _ -> ()
        }

    [<Extension>]
    static member WithCallback(task: Async<'T>, callback: 'T -> unit) : Async<unit> =
        async {
            let! result = task
            callback(result)
        }

    [<Extension>]
    static member Queue(task: Async<'T>, queue: Async.Queue<Async<'T>, unit>) : unit = queue.Request(task, ignore)

type ProgressCallback = TaskProgress -> unit

module TaskProgress_ =

    let log_progress_bar label =
        let mutable download_step = -1

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
