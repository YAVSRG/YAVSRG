namespace Prelude.Tests.Database

open NUnit.Framework
open Percyqaz.Common
open Prelude
open Prelude.Data.User

module DbScores =

    [<Test>]
    let RoundTrip () =
        let db, conn = in_memory ()

        let score_1 =
            {
                Timestamp = Timestamp.now ()
                Rate = 1.0f<rate>
                Mods = Map.empty
                Replay = [| 0uy |]
                IsImported = false
                IsFailed = true
                Keys = 4
            }

        let score_2 =
            {
                Timestamp = Timestamp.now ()
                Rate = 1.2f<rate>
                Mods = Map.empty
                Replay = [| 1uy |]
                IsImported = true
                IsFailed = false
                Keys = 4
            }

        DbScores.save "roundtrip" score_1 db
        DbScores.save "roundtrip" score_2 db

        let result = DbScores.by_chart_id "roundtrip" db

        Assert.AreEqual(2, result.Length)
        Assert.Contains(score_1, result)
        Assert.Contains(score_2, result)

        Assert.AreEqual(0, (DbScores.by_chart_id "doesntexist" db).Length)

        conn.Dispose()

    [<Test>]
    let Batch_RoundTrip () =
        let db, conn = in_memory ()

        let score_1 =
            {
                Timestamp = Timestamp.now ()
                Rate = 1.0f<rate>
                Mods = Map.empty
                Replay = [| 0uy |]
                IsImported = false
                IsFailed = true
                Keys = 4
            }

        let score_2 =
            {
                Timestamp = Timestamp.now ()
                Rate = 1.2f<rate>
                Mods = Map.empty
                Replay = [| 1uy |]
                IsImported = true
                IsFailed = false
                Keys = 4
            }

        DbScores.save_batch [ "batch_roundtrip", score_1; "batch_roundtrip", score_2 ] db

        let result = DbScores.by_chart_id "batch_roundtrip" db

        Assert.AreEqual(2, result.Length)
        Assert.Contains(score_1, result)
        Assert.Contains(score_2, result)

        Assert.AreEqual(0, (DbScores.by_chart_id "doesntexist" db).Length)

        conn.Dispose()

    [<Test>]
    let DeleteByTimestamp () =
        let db, conn = in_memory ()

        let ts = Timestamp.now () - 500L

        let score_1 =
            {
                Timestamp = ts
                Rate = 1.0f<rate>
                Mods = Map.empty
                Replay = [| 0uy |]
                IsImported = false
                IsFailed = true
                Keys = 4
            }

        let score_2 =
            {
                Timestamp = Timestamp.now ()
                Rate = 1.2f<rate>
                Mods = Map.empty
                Replay = [| 1uy |]
                IsImported = true
                IsFailed = false
                Keys = 4
            }

        DbScores.save "delete" score_1 db
        DbScores.save "delete" score_2 db

        Assert.NotZero(DbScores.delete_by_timestamp "delete" ts db)
        Assert.Zero(DbScores.delete_by_timestamp "delete" (ts - 500L) db)
        Assert.Zero(DbScores.delete_by_timestamp "doesntexist" ts db)

        let result = DbScores.by_chart_id "delete" db

        Assert.AreEqual(1, result.Length)
        Assert.Contains(score_2, result)

        conn.Dispose()

    [<Test>]
    let GetBetween_NoResults() =
        let db, conn = in_memory ()

        let score_1 =
            {
                Timestamp = 20L
                Rate = 1.0f<rate>
                Mods = Map.empty
                Replay = [| 0uy |]
                IsImported = false
                IsFailed = true
                Keys = 4
            }

        let score_2 =
            {
                Timestamp = 40L
                Rate = 1.2f<rate>
                Mods = Map.empty
                Replay = [| 1uy |]
                IsImported = true
                IsFailed = false
                Keys = 4
            }

        DbScores.save "between" score_1 db
        DbScores.save "between" score_2 db

        let result = DbScores.get_between 0L 0L db
        Assert.AreEqual(0, result.Length)

        conn.Dispose()

    [<Test>]
    let GetBetween_CorrectResult() =
        let db, conn = in_memory ()

        let score_1 =
            {
                Timestamp = 20L
                Rate = 1.0f<rate>
                Mods = Map.empty
                Replay = [| 0uy |]
                IsImported = false
                IsFailed = true
                Keys = 4
            }

        let score_2 =
            {
                Timestamp = 40L
                Rate = 1.2f<rate>
                Mods = Map.empty
                Replay = [| 1uy |]
                IsImported = true
                IsFailed = false
                Keys = 4
            }

        DbScores.save "between" score_1 db
        DbScores.save "between" score_2 db

        let result = DbScores.get_between 20L 30L db
        Assert.AreEqual(1, result.Length)
        Assert.Contains(("between", score_1), result)

        conn.Dispose()

    [<Test>]
    let Transfer() =
        let db, conn = in_memory ()

        let score_1 =
            {
                Timestamp = 20L
                Rate = 1.0f<rate>
                Mods = Map.empty
                Replay = [| 0uy |]
                IsImported = false
                IsFailed = true
                Keys = 4
            }

        let score_2 =
            {
                Timestamp = 40L
                Rate = 1.2f<rate>
                Mods = Map.empty
                Replay = [| 1uy |]
                IsImported = true
                IsFailed = false
                Keys = 4
            }

        DbScores.save "transfer_a" score_1 db
        DbScores.save "transfer_b" score_2 db

        let result = DbScores.transfer "transfer_a" "transfer_b" db
        Assert.AreEqual(1, result)

        let scores_a = DbScores.by_chart_id "transfer_a" db
        Assert.IsEmpty(scores_a)

        let scores_b = DbScores.by_chart_id "transfer_b" db
        Assert.AreEqual(2, scores_b.Length)

        conn.Dispose()