open NUnit.Framework

open System
open Interlude.Web.Server.Domain

[<SetUpFixture>]
type Setup() =

    let mutable keep_in_memory_db_alive: IDisposable option = None

    [<OneTimeSetUp>]
    member _.SetupInMemoryDatabase() =
        keep_in_memory_db_alive <- Some <| Database.startup_unit_tests ()

    [<OneTimeTearDown>]
    member _.DropInMemoryDatabase() =
        keep_in_memory_db_alive |> Option.iter (fun d -> d.Dispose())

module Program =

    [<EntryPoint>]
    let main _ = 0
