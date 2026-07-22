namespace Prelude.Tests.Stats

open System
open System.Linq
open NUnit.Framework
open Percyqaz.Common
open Prelude.Data.User
open Prelude.Data.User.Stats
open Prelude.Tests.Database
open Prelude.Tests.Helpers

module StatsTests =

    [<Test>]
    let BasicUsage () : unit =
        let db, conn = InMemoryDatabase.Create()
        let user_db = UserDatabase.CreateLazyLoaded(db)
        let clock = VirtualClock(Timestamp.now())
        let stats = Stats.FromUserDatabase(user_db, clock)

        let assert_initial_zeros () : unit =
            Assert.AreEqual(0.0, stats.PlayTime)
            Assert.AreEqual(0.0, stats.PracticeTime)
            Assert.AreEqual(0.0, stats.GameTime)
            Assert.AreEqual(0, stats.NotesHit)
            Assert.AreEqual(0, stats.PlaysStarted)
            Assert.AreEqual(0, stats.PlaysRetried)
            Assert.AreEqual(0, stats.PlaysCompleted)
            Assert.AreEqual(0, stats.PlaysQuit)
            Assert.AreEqual(0L, stats.XP)
            Assert.AreEqual(Map.empty<int, float>, stats.GetCurrentSession().KeymodePlaytime)

        let test_adding_stats () : unit =
            stats.CompletePlay(4, 60_000.0, 2500)
            stats.AddPracticeTime(120_000.0, 2500)
            stats.RetryPlay(7, 10_000.0, 500)
            stats.QuitOutOfPlay(7, 15_000.0, 750)

            Assert.AreEqual(2500 + 2500 + 500 + 750, stats.NotesHit)
            Assert.AreEqual(stats.GetCurrentSession().NotesHit, stats.NotesHit)
            Assert.AreEqual(60_000.0 + 10_000.0 + 15_000.0, stats.PlayTime)
            Assert.AreEqual(stats.GetCurrentSession().PlayTime, stats.PlayTime)
            Assert.AreEqual(120_000.0, stats.PracticeTime)
            Assert.AreEqual(stats.GetCurrentSession().PracticeTime, stats.PracticeTime)
            Assert.AreEqual(Some(60_000.0), stats.GetCurrentSession().KeymodePlaytime.TryFind(4))
            Assert.AreEqual(Some(25_000.0), stats.GetCurrentSession().KeymodePlaytime.TryFind(7))
            Assert.AreEqual(None, stats.GetCurrentSession().KeymodePlaytime.TryFind(10))

        let session_time_cutoff () : unit =
            let today = clock.Today()

            stats.StartOrContinueSession()
            Assert.AreEqual(Map.empty<DateOnly, Session list>, stats.GetPreviousSessions())
            Assert.True(stats.GetCurrentSession().NotesHit > 0)

            clock.Add(SESSION_TIMEOUT)
            stats.StartOrContinueSession()
            Assert.AreEqual(Map.empty<DateOnly, Session list>, stats.GetPreviousSessions())
            Assert.True(stats.GetCurrentSession().NotesHit > 0)

            clock.Add(SESSION_TIMEOUT + 1L)
            stats.StartOrContinueSession()
            Assert.AreEqual(1, stats.GetPreviousSessionsForDate(today).Length)
            Assert.AreEqual(0, stats.GetCurrentSession().NotesHit)
            Assert.AreEqual(2500 + 2500 + 500 + 750, stats.NotesHit)

        let database_roundtrip () : unit =
            let round_tripped_stats = Stats.FromUserDatabase(user_db, clock)
            printfn "%A" (round_tripped_stats.GetPreviousSessions())

            Assert.AreEqual(1, round_tripped_stats.GetPreviousSessions().Keys.Count)

            Assert.AreEqual(
                1,
                round_tripped_stats
                    .GetPreviousSessionsForDate(round_tripped_stats.GetPreviousSessions().Keys.First())
                    .Length
            )

            printfn
                "%A"
                (round_tripped_stats.GetPreviousSessionsForDate(round_tripped_stats.GetPreviousSessions().Keys.First()))

            Assert.AreEqual(stats.GetCurrentSession().LastPlay, round_tripped_stats.GetCurrentSession().LastPlay)

        assert_initial_zeros()
        test_adding_stats()
        session_time_cutoff()
        database_roundtrip()

        conn.Dispose()
