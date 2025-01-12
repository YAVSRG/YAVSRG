namespace Interlude.Web.Server.Domain

open System
open Percyqaz.Common
open Percyqaz.Data.Sqlite
open Interlude.Web.Server

module Migrations =

    open Interlude.Web.Server.Domain.Core

    let run_core (db: Database) : unit =
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
            (fun _ -> Logging.Info("Redis no longer exists to migrate data from"))
            db

        Database.migrate
            "AddTableRatings"
            (fun db ->
                TableRating.CREATE_TABLE.Execute () db |> expect |> ignore
                Logging.Info("Migration created table ratings")
            )
            db

        Database.migrate
            "RulesetSCHasNewId"
            (fun db ->
                let affected_rows = Database.exec_raw """UPDATE scores SET RulesetId = 'SAE1C74D1' WHERE RulesetId = 'SC(J4)548E5A';""" db |> expect
                Logging.Info "Migrated %i old SC J4 records to new ruleset ID (but didn't recalculate the ones on rates)" affected_rows

                let affected_rows = Database.exec_raw """UPDATE leaderboards SET RulesetId = 'SAE1C74D1' WHERE RulesetId = 'SC(J4)548E5A';""" db |> expect
                Logging.Info "Migrated %i leaderboards to new ruleset ID" affected_rows
            )
            db

    open Interlude.Web.Server.Domain.Backbeat

    let run_backbeat (db: Database) : unit =
        Database.migrate
            "InitialTableLevelsAndSuggestions"
            (fun db ->
                TableLevel.CREATE_TABLES.Execute () db |> expect |> ignore
                TableSuggestion.CREATE_TABLE.Execute () db |> expect |> ignore
            )
            db

        Database.migrate
            "ImportOldCrescentLevels"
            (fun _ -> Logging.Info("TablesV1 no longer exist to migrate data from"))
            db

        Database.migrate "AddSources" (fun db -> Source.CREATE_TABLE.Execute () db |> expect |> ignore) db
        Database.migrate "AddSongsAndCharts" (fun db -> Songs.CREATE_TABLES.Execute () db |> expect |> ignore) db

        Database.migrate
            "MoveBackbeatData"
            (fun _ -> Logging.Info("Old backbeat chart dump no longer exists to migrate data from"))
            db

        Database.migrate
            "ResetChartOrigins"
            (fun db -> Database.exec_raw """UPDATE charts SET Sources = '[]';""" db |> expect |> ignore)
            db

module Database =

    let startup () =
        core_db <- Database.from_file ("./data/core.db")
        backbeat_db <- Database.from_file ("./data/backbeat.db")
        Migrations.run_core core_db
        Migrations.run_backbeat backbeat_db

    let startup_unit_tests () : IDisposable =
        let _core_db, keep_alive = Database.in_memory ("unit_tests_core")
        let _backbeat_db, keep_alive_2 = Database.in_memory ("unit_tests_backbeat")
        core_db <- _core_db
        backbeat_db <- _backbeat_db
        Migrations.run_core _core_db
        Migrations.run_backbeat _backbeat_db

        { new IDisposable with
            override this.Dispose() =
                keep_alive.Dispose()
                keep_alive_2.Dispose()
        }