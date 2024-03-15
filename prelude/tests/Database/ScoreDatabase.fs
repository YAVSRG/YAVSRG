namespace Prelude.Tests.Database

open NUnit.Framework
open Percyqaz.Common
open Prelude.Data

module ScoreDatabase =

    [<Test>]
    let BasicRoundTrip () =
        let db, conn = in_memory ()

        let score_db = ScoreDatabase.create false db

        let example_handle = ScoreDatabase.get "example" score_db

        example_handle.Offset <- 5.0f<ms>
        example_handle.Offset <- 10.0f<ms>

        printfn "%A" (DbChartData.get "example" db)

        ScoreDatabase.save_changes score_db

        Assert.True(List.isEmpty example_handle.Scores)

        let score =
            {
                Timestamp = Timestamp.now ()
                Rate = 1.0f
                Mods = Map.empty
                Replay = [| 0uy |]
                IsImported = false
                Keys = 4
            }

        ScoreDatabase.save_score "example" score score_db

        Assert.False(List.isEmpty example_handle.Scores)

        ScoreDatabase.save_changes score_db

        Assert.NotZero((DbScores.by_chart_id "example" db).Length)
        printfn "%A" (DbChartData.get "example" db)

        conn.Dispose()
