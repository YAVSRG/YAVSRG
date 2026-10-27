namespace Prelude.Data.Imports

open Percyqaz.Common
open Prelude.Formats.Conversions

[<AutoOpen>]
module QueueServices =

    let import_queue =
        { new Async.Queue<Async<Result<ConversionResult, string>>, Result<ConversionResult, string>>() with
            override this.Handle
                (task: Async<Result<ConversionResult, string>>)
                : Async<Result<ConversionResult, string>> =
                task
        }

    let import_queue_new =
        { new Async.Queue<Async<unit>, unit>() with
            override this.Handle(task: Async<unit>) : Async<unit> = task
        }

    let general_task_queue =
        { new Async.Queue<Async<unit>, unit>() with
            override this.Handle(task: Async<unit>) : Async<unit> = task
        }
