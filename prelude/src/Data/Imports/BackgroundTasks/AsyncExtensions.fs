namespace Prelude.Data.Imports

open System.Runtime.CompilerServices
open Percyqaz.Common

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
