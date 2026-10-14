namespace Prelude.Tests.Database

open System
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open NUnit.Framework
open Percyqaz.Data.Sqlite
open Prelude.Data.UserDatabase
open Prelude.Data.ChartDatabase

[<SetUpFixture>]
type Setup() =

    let mutable conn: IDisposable = Unchecked.defaultof<_>

    [<OneTimeSetUp>]
    member _.Setup() =
        let db, _conn = Database.in_memory "interlude"

        // migrate database (same database can have the tables that are normally split over 2 databases)
        UserDatabase.CreateLazyLoaded(db) |> ignore
        ChartDatabase.CreateLazyLoaded(db) |> ignore
        conn <- _conn // in-memory database persists until teardown where it gets disposed

    [<OneTimeTearDown>]
    member _.Teardown() = conn.Dispose()

type InMemoryDatabase =
    static member Create() : Database * _ = Database.in_memory "interlude"

    static member CreateIsolated([<CallerMemberName; Optional; DefaultParameterValue("")>] id: string) : Database * _ =
        let db, conn = Database.in_memory("interlude_" + id)
        UserDatabase.CreateLazyLoaded(db) |> ignore
        ChartDatabase.CreateLazyLoaded(db) |> ignore
        db, conn
