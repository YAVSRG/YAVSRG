namespace Percyqaz.Flux.Windowing

open System.Threading

(*
    Action queuing
    The game has:
    - A "window thread" that handles input and window events
    - A separate "game thread" that handles rendering and game logic

    Often another background thread needs to queue up an action to be executed on the game thread
    Also commonly used to defer an action to happen at the start of next frame even when already on the game thread

    Deferred actions are fire-and-forget, they will execute in the order they are queued, when the thread gets around to it
*)

type ThreadActionQueue() =

    let mutable managed_thread_id = -1
    let mutable action_queue : (unit -> unit) list = []

    member internal this.Bind(id: int) : unit =
        managed_thread_id <- id

    member internal this.BindToCurrent() : unit = this.Bind(Thread.CurrentThread.ManagedThreadId)

    member internal this.RunQueue() : unit =
        assert(this.IsCurrent())
        lock this (fun () ->
            while not (List.isEmpty action_queue) do
                let actions = action_queue
                action_queue <- []
                (for action in actions do action())
        )

    member this.IsCurrent() : bool =
        Thread.CurrentThread.ManagedThreadId = managed_thread_id

    member this.Defer(action: unit -> unit) : unit =
        lock this (fun () -> action_queue <- action_queue @ [ action ])

    member this.EnsureCurrent(action: unit -> unit) =
        if this.IsCurrent() then action () else this.Defer action