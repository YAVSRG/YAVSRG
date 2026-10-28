namespace Prelude.Data.Imports

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
