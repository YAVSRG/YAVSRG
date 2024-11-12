namespace Prelude.Tests.Database

open NUnit.Framework
open Percyqaz.Common
open Percyqaz.Data
open Prelude
open Prelude.Data.User

module DbSingletons =

    [<Json.AutoCodec>]
    type TestDataType =
        {
            Timestamp: int64
            Rate: float32<rate>
            Mods: int64 list
            Replay: byte array
            IsImported: bool
            IsFailed: bool
            Keys: int
        }

    [<Test>]
    let RoundTrip () =
        let db, conn = in_memory ()

        let data =
            {
                Timestamp = Timestamp.now ()
                Rate = 1.0f<rate>
                Mods = [ 17L ]
                Replay = [| 0uy |]
                IsImported = false
                IsFailed = true
                Keys = 4
            }

        DbSingletons.save<TestDataType> "roundtrip" data db

        let result = 
            DbSingletons.get_or_default 
                "roundtrip"
                {
                    Timestamp = Timestamp.now ()
                    Rate = 2.0f<rate>
                    Mods = [ 18L ]
                    Replay = [| 1uy |]
                    IsImported = true
                    IsFailed = false
                    Keys = 4
                }
                db

        Assert.AreEqual(data, result)

        conn.Dispose()
    
    
    [<Test>]
    let Overwriting () =
        let db, conn = in_memory ()

        let DEFAULT =
            {
                Timestamp = Timestamp.now ()
                Rate = 2.0f<rate>
                Mods = [ 18L ]
                Replay = [| 1uy |]
                IsImported = true
                IsFailed = false
                Keys = 4
            }

        let data =
            {
                Timestamp = Timestamp.now ()
                Rate = 1.0f<rate>
                Mods = [ 17L ]
                Replay = [| 0uy |]
                IsImported = false
                IsFailed = true
                Keys = 4
            }

        DbSingletons.save<TestDataType> "overwriting" DEFAULT db
        DbSingletons.save<TestDataType> "overwriting" data db

        let result = 
            DbSingletons.get_or_default 
                "overwriting"
                DEFAULT
                db

        Assert.AreEqual(data, result)
        Assert.AreNotEqual(DEFAULT, result)

        conn.Dispose()

    [<Test>]
    let DoesntExist () =
        let db, conn = in_memory ()

        let DEFAULT =
            {
                Timestamp = Timestamp.now ()
                Rate = 1.0f<rate>
                Mods = [ 18L ]
                Replay = [| 0uy |]
                IsImported = false
                IsFailed = true
                Keys = 4
            }

        let result = 
            DbSingletons.get_or_default 
                "doesntexist"
                DEFAULT
                db

        Assert.AreEqual(DEFAULT, result)

        conn.Dispose()