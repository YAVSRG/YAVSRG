namespace Interlude.Web.Server.Domain.New

open System
open Percyqaz.Common
open Percyqaz.Data.Sqlite
open Interlude.Web.Server

module Migrations =

    let run (db: Database) =
        Database.migrate 
            "InitialTables" 
            (fun db ->
                Database.create_table User.TABLE db |> expect |> ignore
                Friends.CREATE_TABLE.Execute () db |> expect |> ignore
                Replay.CREATE_TABLE.Execute () db |> expect |> ignore
                Score.CREATE_TABLE.Execute () db |> expect |> ignore
                Logging.Info("Migration #1 complete")
            )
            db

module Database =

    let startup() =
        if IO.File.Exists("./data/core.db") then IO.File.Delete("./data/core.db") // for debug purposes
        let _db = Database.from_file("./data/core.db")
        Migrations.run _db
        db <- db

    let startup_unit_tests() : IDisposable =
        let _db, keep_alive = Database.in_memory("unit_tests")
        Migrations.run _db
        db <- _db
        keep_alive