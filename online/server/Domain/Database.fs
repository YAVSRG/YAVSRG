namespace Interlude.Web.Server.Domain

open System
open Percyqaz.Common
open Percyqaz.Data.Sqlite
open Interlude.Web.Server

module Migrations =

    open Interlude.Web.Server.Domain.Objects

    let run (db: Database) =
        Database.migrate 
            "InitialTables" 
            (fun db ->
                Database.create_table User.TABLE db |> expect |> ignore
                Friends.CREATE_TABLE.Execute () db |> expect |> ignore
                Replay.CREATE_TABLE.Execute () db |> expect |> ignore
                Score.CREATE_TABLE.Execute () db |> expect |> ignore
                Leaderboard.CREATE_TABLE.Execute () db |> expect |> ignore
                Logging.Info("Migration created initial tables")
            )
            db
        Database.migrate
            "MigrateEverythingFromRedis"
            (fun db -> Logging.Info("Redis no longer exists to migrate data from"))
            db

module Database =

    let startup() =
        db <- Database.from_file("./data/core.db")
        Migrations.run db

    let startup_unit_tests() : IDisposable =
        let _db, keep_alive = Database.in_memory("unit_tests")
        db <- _db
        Migrations.run _db
        keep_alive