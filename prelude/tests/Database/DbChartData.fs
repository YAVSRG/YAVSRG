namespace Prelude.Tests.Database

open NUnit.Framework
open Percyqaz.Common
open Prelude
open Prelude.Gameplay
open Prelude.Data.User

module DbChartData =

    [<Test>]
    let Get_DoesntExist () =
        let db, conn = in_memory ()

        let result = DbChartData.get "doesntexist" db
        Assert.AreEqual(DbChartData.DEFAULT, result)

        conn.Dispose()

    [<Test>]
    let Offset_RoundTrip () =
        let db, conn = in_memory ()

        DbChartData.save_offsets [ "offset", 5.0f<ms> ] db

        Assert.AreEqual(
            DbChartData.get "offset" db,
            { DbChartData.DEFAULT with
                Offset = 5.0f<ms>
            }
        )

        Assert.AreEqual(DbChartData.DEFAULT, DbChartData.get "doesntexist" db)

        conn.Dispose()

    [<Test>]
    let LastPlayed_RoundTrip () =
        let db, conn = in_memory ()

        let now = Timestamp.now ()

        DbChartData.save_last_played [ "lastplayed", now ] db

        Assert.AreEqual(
            DbChartData.get "lastplayed" db,
            { DbChartData.DEFAULT with
                LastPlayed = now
            }
        )

        Assert.AreEqual(DbChartData.DEFAULT, DbChartData.get "doesntexist" db)

        conn.Dispose()

    [<Test>]
    let Comment_RoundTrip () =
        let db, conn = in_memory ()

        DbChartData.save_comments [ "comment1", "Comment on chart 1"; "comment2", "Comment on chart 2" ] db

        Assert.AreEqual(
            DbChartData.get "comment2" db,
            { DbChartData.DEFAULT with
                Comment = "Comment on chart 2"
            }
        )

        Assert.AreEqual(DbChartData.DEFAULT, DbChartData.get "doesntexist" db)

        conn.Dispose()

    [<Test>]
    let Breakpoints_RoundTrip () =
        let db, conn = in_memory ()

        DbChartData.save_breakpoints [ "breakpoints", [ 0.0f<ms>; 1.0f<ms>; 2.0f<ms> ] ] db

        Assert.AreEqual(
            DbChartData.get "breakpoints" db,
            { DbChartData.DEFAULT with
                Breakpoints = [ 0.0f<ms>; 1.0f<ms>; 2.0f<ms> ]
            }
        )
        
        Assert.AreEqual(DbChartData.DEFAULT, DbChartData.get "doesntexist" db)

        conn.Dispose()

    [<Test>]
    let Bests_RoundTrip () =
        let db, conn = in_memory ()

        let bests =
            (Map.ofList
                [
                    "SCJ4",
                    {
                        Bests.Accuracy = PersonalBests.Empty
                        Grade = PersonalBests.Empty
                        Lamp = PersonalBests.Empty
                    }
                ])

        DbChartData.save_personal_bests [ "bests", bests ] db

        Assert.AreEqual(
            DbChartData.get "bests" db,
            { DbChartData.DEFAULT with
                PersonalBests = bests
            }
        )
        
        Assert.AreEqual(DbChartData.DEFAULT, DbChartData.get "doesntexist" db)

        conn.Dispose()
