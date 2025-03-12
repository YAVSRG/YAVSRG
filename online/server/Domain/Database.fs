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
                Score.CREATE_TABLE_OLD.Execute () db |> expect |> ignore
                Replay.CREATE_TABLE_OLD.Execute () db |> expect |> ignore
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
            (fun _ -> Logging.Info("Original tables to apply ruleset ID change to no longer exists"))
            db

        Database.migrate
            "LeaderboardAllBackbeatCharts"
            (fun db ->
                Database.exec_raw """DROP TABLE IF EXISTS leaderboards;""" db |> expect |> ignore
                Replay.CREATE_TABLE.Execute () db |> expect |> ignore
                Score.CREATE_TABLE.Execute () db |> expect |> ignore
            )
            db

        Database.migrate
            "MigrateLeaderboardData"
            (fun db -> Score.MIGRATE_OLD_TO_NEW.Execute () db |> expect |> Logging.Debug "Migrated %i scores + replays in prep of leaderboards")
            db

        Database.migrate
            "ClearOldScoreTables"
            (fun db ->
                Database.exec_raw """DROP TABLE IF EXISTS scores;""" db |> expect |> ignore
                Database.exec_raw """DROP TABLE IF EXISTS replays;""" db |> expect |> ignore
            )
            db

        Database.migrate
            "UnrankPreLeaderboardScores"
            (fun db ->
                Database.exec_raw """UPDATE scores2 SET Ranked = 0 WHERE ReplayId IS NULL;""" db |> expect |> Logging.Debug "Unranked %i scores in prep of leaderboards"
            )
            db

        Database.migrate
            "CreateStatsTable"
            (fun db -> Stats.CREATE_TABLE.Execute () db |> expect |> ignore)
            db

        Database.migrate
            "CreateMonthlyStatsTable"
            (fun db -> MonthlyStats.CREATE_TABLE.Execute () db |> expect |> ignore)
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

        Database.migrate
            "ResetChartOrigins2"
            (fun db ->
                Database.exec_raw
                    """
                    UPDATE charts
                    SET Sources =
                    (
	                    SELECT coalesce('[' || group_concat(json_each.value) || ']', '[]')
	                    FROM json_each(charts.Sources)
	                    WHERE json_each.value NOT LIKE '%Osu%'
	                    AND json_each.value NOT LIKE '%Quaver%'
                    );
                """ db |> expect |> ignore)
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