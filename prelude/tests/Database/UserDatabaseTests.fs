namespace Prelude.Tests.Database

open NUnit.Framework
open Percyqaz.Common
open Prelude
open Prelude.Data.User

module UserDatabaseTests =

    [<Test>]
    let BasicRoundTrip () =
        let db, conn = InMemoryDatabase.Create()

        let user_db = UserDatabase.CreateLazyLoaded(db)

        let example_handle = user_db.GetChartData("example")

        example_handle.Offset <- 5.0f<ms>
        example_handle.Offset <- 10.0f<ms>

        printfn "%A" (DbChartData.get "example" db)

        user_db.SaveChanges()

        Assert.True(List.isEmpty example_handle.Scores)

        let score =
            {
                Timestamp = Timestamp.now()
                Rate = 1.0f<rate>
                Mods = Map.empty
                Replay = [| 0uy |]
                IsImported = false
                IsFailed = false
                Keys = 4
            }

        user_db.SaveScore("example", score)

        Assert.False(List.isEmpty example_handle.Scores)

        user_db.SaveChanges()

        Assert.NotZero((DbScores.by_chart_id "example" db).Length)
        printfn "%A" (DbChartData.get "example" db)

        conn.Dispose()
