namespace Interlude.Web.Server.Domain

open System
open Percyqaz.Common
open Percyqaz.Data.Sqlite
open Interlude.Web.Server
open Interlude.Web.Server.Domain.New

module Migrations_New =

    let run (db: Database) =
        Database.migrate 
            "InitialUsersAndFriends" 
            (fun db ->
                Database.create_table User.TABLE db |> expect |> ignore
                Friends.CREATE_TABLE.Execute () db |> expect |> ignore
                Replay.CREATE_TABLE.Execute () db |> expect |> ignore
                Logging.Info("Migration #1 complete")
            )
            db

module Database =

    let startup() =
        let _db = Database.from_file("./data/core.db")
        Migrations_New.run _db
        db <- db

    let startup_unit_tests() : IDisposable =
        let _db, keep_alive = Database.in_memory("unit_tests")
        Migrations_New.run _db
        db <- _db
        keep_alive