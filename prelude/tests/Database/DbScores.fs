namespace Prelude.Tests.Database

open NUnit.Framework
open Percyqaz.Common
open Prelude
open Prelude.Gameplay
open Prelude.Data
open Prelude.Data.Scores

module DbScores =

    [<Test>]
    let RoundTrip() =
        let db, conn = DatabaseSetup.in_memory()

        let score_1 = 
            {
                Timestamp = Timestamp.now()
                Rate = 1.0f
                Mods = Map.empty
                Replay = [|0uy|]
                IsImported = false
                Keys = 4
            }
        let score_2 =
            {
                Timestamp = Timestamp.now()
                Rate = 1.2f
                Mods = Map.empty
                Replay = [|1uy|]
                IsImported = true
                Keys = 4
            }

        DbScore.save "roundtrip" score_1 db
        DbScore.save "roundtrip" score_2 db

        let result = DbScore.by_chart_id "roundtrip" db

        Assert.AreEqual(2, result.Length)
        Assert.Contains(score_1, result)
        Assert.Contains(score_2, result)

        Assert.AreEqual(0, (DbScore.by_chart_id "doesntexist" db).Length)

        conn.Dispose()

    [<Test>]
    let Batch_RoundTrip() =
        let db, conn = DatabaseSetup.in_memory()
    
        let score_1 = 
            {
                Timestamp = Timestamp.now()
                Rate = 1.0f
                Mods = Map.empty
                Replay = [|0uy|]
                IsImported = false
                Keys = 4
            }
        let score_2 =
            {
                Timestamp = Timestamp.now()
                Rate = 1.2f
                Mods = Map.empty
                Replay = [|1uy|]
                IsImported = true
                Keys = 4
            }
    
        DbScore.save_batch ["roundtrip", score_1; "roundtrip", score_2] db
    
        let result = DbScore.by_chart_id "roundtrip" db
    
        Assert.AreEqual(2, result.Length)
        Assert.Contains(score_1, result)
        Assert.Contains(score_2, result)
    
        Assert.AreEqual(0, (DbScore.by_chart_id "doesntexist" db).Length)
    
        conn.Dispose()

    [<Test>]
    let DeleteByTimestamp() =
        let db, conn = DatabaseSetup.in_memory()
        
        let ts = Timestamp.now() - 500L

        let score_1 = 
            {
                Timestamp = ts
                Rate = 1.0f
                Mods = Map.empty
                Replay = [|0uy|]
                IsImported = false
                Keys = 4
            }
        let score_2 =
            {
                Timestamp = Timestamp.now()
                Rate = 1.2f
                Mods = Map.empty
                Replay = [|1uy|]
                IsImported = true
                Keys = 4
            }
        
        DbScore.save "delete" score_1 db
        DbScore.save "delete" score_2 db

        Assert.NotZero(DbScore.delete_by_timestamp "delete" ts db)
        Assert.Zero(DbScore.delete_by_timestamp "delete" (ts - 500L) db)
        Assert.Zero(DbScore.delete_by_timestamp "doesntexist" ts db)
        
        let result = DbScore.by_chart_id "delete" db
        
        Assert.AreEqual(1, result.Length)
        Assert.Contains(score_2, result)
        
        conn.Dispose()