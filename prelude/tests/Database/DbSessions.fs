namespace Prelude.Tests.Database

open NUnit.Framework
open Prelude.Calculator
open Prelude.Data.User

module DbSessions =

    [<Test>]
    let Empty () =
        let db, conn = in_memory ()

        let result = DbSessions.get_all db
        Assert.IsEmpty(result)

    [<Test>]
    let RoundTrip () =
        let db, conn = in_memory ()

        let session : Session =
            {
                Start = 364L
                End = 365L

                PlayTime = 10.0
                PracticeTime = 10.0
                GameTime = 30.0
                NotesHit = 256

                PlaysStarted = 2
                PlaysRetried = 1
                PlaysQuit = 2
                PlaysCompleted = 1

                XP = 567L
                KeymodeSkills = Array.init 8 (fun _ -> KeymodeSkillBreakdown.Default.Tiny)
                KeymodePlaytime = Map.ofSeq [4, 3600.0; 7, 1800.0]
            }

        DbSessions.save session db
        let result = DbSessions.get_all db
        Assert.IsNotEmpty(result)
        Assert.AreEqual(session, result.[0])
        Assert.AreEqual(1, result.Length)

    [<Test>]
    let Idempotence () =
        let db, conn = in_memory ()

        let session : Session =
            {
                Start = 364L
                End = 365L

                PlayTime = 10.0
                PracticeTime = 10.0
                GameTime = 30.0
                NotesHit = 256

                PlaysStarted = 2
                PlaysRetried = 1
                PlaysQuit = 2
                PlaysCompleted = 1

                XP = 567L
                KeymodeSkills = Array.init 8 (fun _ -> KeymodeSkillBreakdown.Default.Tiny)
                KeymodePlaytime = Map.ofSeq [4, 3600.0; 7, 1800.0]
            }

        DbSessions.save session db
        DbSessions.save session db
        DbSessions.save session db

        let result = DbSessions.get_all db
        Assert.IsNotEmpty(result)
        Assert.AreEqual(session, result.[0])
        Assert.AreEqual(1, result.Length)