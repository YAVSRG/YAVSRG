namespace Prelude.Data

// todo: Prelude.Data.Imports namespace ?

type TaskProgress =
    | Generic of label: string
    | Downloading of percent: float32
    | Processing of count: int * total: int
    | Nested of label: string * count: int * total: int * inner: TaskProgress

type TaskStatus =
    | InProgress of TaskProgress
    | Faulted
    | Completed
