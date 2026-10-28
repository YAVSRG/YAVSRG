namespace Interlude.Features.Import

open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Prelude.Data.Imports

module TaskTracking =

    let list = ResizeArray<TaskContext>()

    let add (label: string) : TaskContext =
        assert GameThread.is_game_thread()

        let status = TaskContext(label)
        list.Add(status)
        status

    let in_progress () : bool =
        import_queue.Status <> Async.QueueStatus.Idle || general_task_queue.Status <> Async.QueueStatus.Idle

    let current_progress () : TaskStatus =
        list
        |> Seq.map _.Status
        |> Seq.tryPick(
            function
            | Completed -> None
            | otherwise -> Some otherwise
        )
        |> Option.defaultValue Completed
