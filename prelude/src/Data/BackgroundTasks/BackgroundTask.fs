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
