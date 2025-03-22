namespace Prelude.Data

open Percyqaz.Common

type TaskProgress =
    | Generic of label: string
    | Downloading of percent: float32
    | Processing of count: int * total: int
    | Faulted
    | Complete
    | Nested of label: string * count: int * total: int * inner: TaskProgress

type ProgressCallback = TaskProgress -> unit

module TaskProgress =

    let log_progress_bar label =
        let mutable download_step = -1
        function
        | Generic status -> Logging.Warn "%s: %s" label status
        | Downloading p ->
            let t = p / 0.1f |> floor |> int
            if t > download_step then
                download_step <- t
                Logging.Info "%s: [%s%s] %.0f%%" label (String.replicate download_step "#") (String.replicate (max 0 (10 - download_step)) "-") (p * 100.0f)
        | Processing (i, count) ->
            Logging.Info "%s: %i / %i" label i count
        | Faulted ->
            Logging.Warn "%s: Faulted!" label
        | Complete ->
            Logging.Info "%s: Complete!" label
        | Nested (nlabel, i, count, inner) ->
            Logging.Info "%s: %s %i/%i [%A]" label nlabel i count inner