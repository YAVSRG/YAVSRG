namespace Prelude.Tests.Database

open NUnit.Framework
open Percyqaz.Common
open Prelude
open Prelude.Data.User

module UserDatabase =

    [<Test>]
    let BasicRoundTrip () =
        let db, conn = in_memory ()

        let score_db = UserDatabase.create false db

        let example_handle = UserDatabase.get_chart_data "example" score_db

        example_handle.Offset <- 5.0f<ms>
        example_handle.Offset <- 10.0f<ms>

        printfn "%A" (DbChartData.get "example" db)

        UserDatabase.save_changes score_db

        Assert.True(List.isEmpty example_handle.Scores)

        let score =
            {
                Timestamp = Timestamp.now ()
                Rate = 1.0f<rate>
                Mods = Map.empty
                Replay = [| 0uy |]
                IsImported = false
                IsFailed = false
                Keys = 4
            }

        UserDatabase.save_score "example" score score_db

        Assert.False(List.isEmpty example_handle.Scores)

        UserDatabase.save_changes score_db

        Assert.NotZero((DbScores.by_chart_id "example" db).Length)
        printfn "%A" (DbChartData.get "example" db)

        conn.Dispose()
