namespace Prelude.Data.Library

open Percyqaz.Common
open Percyqaz.Data.Sqlite

module private ChartDatabaseMigrations =
    let migrate (db: Database) : Database =
        Database.migrate "AddChartsTable" (fun db -> DbCharts.CREATE_TABLE.Execute () db |> expect |> ignore) db
        Database.migrate "ResetOriginsColumn" (fun db -> DbCharts.RESET_ORIGINS.Execute () db |> expect |> ignore) db
        Database.migrate "ResetOriginsColumn2" (fun db -> DbCharts.RESET_ORIGINS.Execute () db |> expect |> ignore) db

        Database.migrate
            "ResetOriginsColumn3"
            (fun db -> DbCharts.RESET_OSU_QUAVER_ORIGINS.Execute () db |> expect |> ignore)
            db

        db