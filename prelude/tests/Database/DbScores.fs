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
                Keys = 4
            }

        let score_2 =
            {
                Timestamp = Timestamp.now ()
                Rate = 1.2f<rate>
                Mods = Map.empty
                Replay = [| 1uy |]
                IsImported = true
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
                Keys = 4
            }

        let score_2 =
            {
                Timestamp = Timestamp.now ()
                Rate = 1.2f<rate>
                Mods = Map.empty
                Replay = [| 1uy |]
                IsImported = true
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
                Keys = 4
            }

        let score_2 =
            {
                Timestamp = Timestamp.now ()
                Rate = 1.2f<rate>
                Mods = Map.empty
                Replay = [| 1uy |]
                IsImported = true
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
