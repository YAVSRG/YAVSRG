﻿namespace Prelude.Tests.Database

open System
open NUnit.Framework
open Percyqaz.Data.Sqlite
open Prelude.Data

[<SetUpFixture>]
type Setup() =

    let mutable conn: IDisposable = Unchecked.defaultof<_>

    [<OneTimeSetUp>]
    member _.Setup() =
        let db, _conn = Database.in_memory "interlude"
        ScoreDatabase.create false db |> ignore // ensures migration
        conn <- _conn // in-memory database persists until teardown where it gets disposed

    [<OneTimeTearDown>]
    member _.Teardown() = conn.Dispose()

[<AutoOpen>]
module Helpers =

    let in_memory () =
        Percyqaz.Data.Sqlite.Database.in_memory "interlude"
