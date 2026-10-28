namespace Prelude.Data.Imports

open Percyqaz.Common

[<AutoOpen>]
module QueueServices =

    let import_queue =
        { new Async.Queue<Async<unit>, unit>() with
            override this.Handle(task: Async<unit>) : Async<unit> = task
        }

    let general_task_queue =
        { new Async.Queue<Async<unit>, unit>() with
            override this.Handle(task: Async<unit>) : Async<unit> = task
        }
