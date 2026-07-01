namespace Prelude.Data.User

open Percyqaz.Common
open Percyqaz.Data.Sqlite

module private UserDatabaseMigrations =

    let migrate (db: Database) : Database =
        Database.migrate "AddScoresTable" (fun db -> DbScores.CREATE_TABLE.Execute () db |> expect |> ignore) db
        Database.migrate "AddChartDataTable" (fun db -> DbChartData.CREATE_TABLE.Execute () db |> expect |> ignore) db

        Database.migrate
            "AddChartIdIndexToScores"
            (fun db -> DbScores.CREATE_INDEX.Execute () db |> expect |> ignore)
            db

        Database.migrate
            "ResetPbDataTimestampsAdded"
            (fun db -> DbChartData.RESET_PERSONAL_BESTS.Execute () db |> expect |> ignore)
            db

        Database.migrate
            "AddIsFailedColumnToScores"
            (fun db -> DbScores.ADD_FAILED_COLUMN.Execute () db |> expect |> ignore)
            db

        Database.migrate
            "AddTimestampIndexToScores"
            (fun db -> DbScores.CREATE_TIMESTAMP_INDEX.Execute () db |> expect |> ignore)
            db

        Database.migrate "AddSingletons" (fun db -> DbSingletons.CREATE_TABLE.Execute () db |> expect |> ignore) db

        Database.migrate
            "AddSessions"
            (fun db ->
                Database.create_table DbSessions.TABLE db |> expect |> ignore
                DbSessions.CREATE_INDEX.Execute () db |> expect |> ignore
            )
            db

        Database.migrate
            "RemoveBrokenSessionJan1970"
            (fun db ->
                Database.exec_raw """DELETE FROM SESSIONS WHERE Start = 0""" db
                |> expect
                |> function
                    | 0 -> ()
                    | n -> Logging.Debug "Removed %i sessions with Start set to 0L" n
            )
            db

        Database.migrate
            "AddKeymodePlaytimeStats"
            (fun db -> DbSessions.ADD_KEYMODE_PLAYTIME.Execute () db |> expect |> ignore)
            db

        Database.migrate
            "RemoveKeymodeSkills"
            (fun db -> DbSessions.REMOVE_KEYMODE_SKILLS.Execute () db |> expect |> ignore)
            db

        db