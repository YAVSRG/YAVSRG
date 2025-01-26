namespace Interlude.Web.Tests.Domain.Core

open NUnit.Framework
open Percyqaz.Common
open Interlude.Web.Server.Domain.Core

module Stats =

    [<Test>]
    let Stats_RoundTrip () =
        let user_id = User.create ("StatsRoundTrip", 0uL) |> User.save_new

        let stats : Stats =
            {
                LastSync = Timestamp.now()
                Playtime = 100.0
                PracticeTime = 50.0
                GameTime = 200.0
                NotesHit = 48376
                XP = 3273564L
                _4KPlaytime = 60.0
                _4K = { Combined = 12000.0f; Jacks = 5000.0f; Chordstream = 4000.0f; Stream = 3000.0f }
                _7KPlaytime = 40.0
                _7K = { Combined = 11000.0f; Jacks = 4500.0f; Chordstream = 3500.0f; Stream = 3000.0f }
                _10KPlaytime = 0.0
                _10K = { Combined = 10500.0f; Jacks = 3500.0f; Chordstream = 4000.0f; Stream = 3000.0f }
            }

        Stats.save user_id stats

        let result = Stats.get user_id

        Assert.AreEqual(Some stats, result)

    [<Test>]
    let Stats_Idempotence () =
        let user_id = User.create ("StatsIdempotence", 0uL) |> User.save_new

        let stats : Stats =
            {
                LastSync = Timestamp.now()
                Playtime = 100.0
                PracticeTime = 50.0
                GameTime = 200.0
                NotesHit = 48376
                XP = 3273564L
                _4KPlaytime = 60.0
                _4K = { Combined = 12000.0f; Jacks = 5000.0f; Chordstream = 4000.0f; Stream = 3000.0f }
                _7KPlaytime = 40.0
                _7K = { Combined = 11000.0f; Jacks = 4500.0f; Chordstream = 3500.0f; Stream = 3000.0f }
                _10KPlaytime = 0.0
                _10K = { Combined = 10500.0f; Jacks = 3500.0f; Chordstream = 4000.0f; Stream = 3000.0f }
            }

        Stats.save user_id stats
        Stats.save user_id stats
        Stats.save user_id stats

        let result = Stats.get user_id

        Assert.AreEqual(Some stats, result)

    [<Test>]
    let Stats_DoesntExist () =
        let user_id = User.create ("StatsDoesntExist", 0uL) |> User.save_new

        let result = Stats.get user_id
        let result_or_default = Stats.get_or_default user_id
        Assert.AreEqual(None, result)
        Assert.AreEqual(Stats.Default, result_or_default)

    [<Test>]
    let Stats_Leaderboards () =
        let user_id = User.create ("StatsLeaderboard", 0uL) |> User.save_new

        let stats : Stats =
            {
                LastSync = Timestamp.now()
                Playtime = 1000.0
                PracticeTime = 500.0
                GameTime = 2000.0
                NotesHit = 483767
                XP = 32735648L
                _4KPlaytime = 600.0
                _4K = { Combined = 12000.0f; Jacks = 5000.0f; Chordstream = 4000.0f; Stream = 3000.0f }
                _7KPlaytime = 400.0
                _7K = { Combined = 11000.0f; Jacks = 4500.0f; Chordstream = 3500.0f; Stream = 3000.0f }
                _10KPlaytime = 10.0
                _10K = { Combined = 10500.0f; Jacks = 3500.0f; Chordstream = 4000.0f; Stream = 3000.0f }
            }

        Stats.save user_id stats

        let result = Stats.xp_leaderboard ()
        printfn "%A" result
        Assert.True(result.Length >= 1)

        let result = Stats.leaderboard_4k ()
        printfn "%A" result
        Assert.True(result.Length >= 1)

        let result = Stats.leaderboard_7k ()
        printfn "%A" result
        Assert.True(result.Length >= 1)

module MonthlyStats =

    [<Test>]
    let MonthlyStats_RoundTrip () =
        let user_id = User.create ("MonthlyStatsRoundTrip", 0uL) |> User.save_new

        let stats : MonthlyStats =
            {
                LastSync = Timestamp.now()
                Playtime = 100.0
                XP = 3273564L
                _4KPlaytime = 60.0
                _4K = { Combined = 12000.0f; Jacks = 5000.0f; Chordstream = 4000.0f; Stream = 3000.0f }
                _7KPlaytime = 40.0
                _7K = { Combined = 11000.0f; Jacks = 4500.0f; Chordstream = 3500.0f; Stream = 3000.0f }
                _10KPlaytime = 0.0
                _10K = { Combined = 10500.0f; Jacks = 3500.0f; Chordstream = 4000.0f; Stream = 3000.0f }
            }

        MonthlyStats.save 1 user_id stats

        let result = MonthlyStats.get 1 user_id

        Assert.AreEqual(Some stats, result)

    [<Test>]
    let MonthlyStats_Idempotence () =
        let user_id = User.create ("MonthlyStatsIdempotence", 0uL) |> User.save_new

        let stats : MonthlyStats =
            {
                LastSync = Timestamp.now()
                Playtime = 100.0
                XP = 3273564L
                _4KPlaytime = 60.0
                _4K = { Combined = 12000.0f; Jacks = 5000.0f; Chordstream = 4000.0f; Stream = 3000.0f }
                _7KPlaytime = 40.0
                _7K = { Combined = 11000.0f; Jacks = 4500.0f; Chordstream = 3500.0f; Stream = 3000.0f }
                _10KPlaytime = 0.0
                _10K = { Combined = 10500.0f; Jacks = 3500.0f; Chordstream = 4000.0f; Stream = 3000.0f }
            }

        MonthlyStats.save 1 user_id stats
        MonthlyStats.save 1 user_id stats
        MonthlyStats.save 1 user_id stats

        let result = MonthlyStats.get 1 user_id

        Assert.AreEqual(Some stats, result)

    [<Test>]
    let MonthlyStats_DoesntExist () =
        let user_id = User.create ("MonthlyStatsDoesntExist", 0uL) |> User.save_new

        let result = MonthlyStats.get 1 user_id
        let result_or_default = MonthlyStats.get_or_default 1 user_id
        Assert.AreEqual(None, result)
        Assert.AreEqual(MonthlyStats.Default, result_or_default)

    [<Test>]
    let MonthlyStats_Leaderboards () =
        let user_id = User.create ("MonthlyStatsLeaderboard", 0uL) |> User.save_new

        let stats : MonthlyStats =
            {
                LastSync = Timestamp.now()
                Playtime = 1000.0
                XP = 32735648L
                _4KPlaytime = 600.0
                _4K = { Combined = 12000.0f; Jacks = 5000.0f; Chordstream = 4000.0f; Stream = 3000.0f }
                _7KPlaytime = 400.0
                _7K = { Combined = 11000.0f; Jacks = 4500.0f; Chordstream = 3500.0f; Stream = 3000.0f }
                _10KPlaytime = 10.0
                _10K = { Combined = 10500.0f; Jacks = 3500.0f; Chordstream = 4000.0f; Stream = 3000.0f }
            }

        MonthlyStats.save 1 user_id stats

        let result = MonthlyStats.xp_leaderboard 1
        printfn "%A" result
        Assert.True(result.Length >= 1)

        let result = MonthlyStats.xp_leaderboard 2
        Assert.AreEqual(0, result.Length)

        let result = MonthlyStats.leaderboard_4k 1
        printfn "%A" result
        Assert.True(result.Length >= 1)

        let result = MonthlyStats.leaderboard_4k 2
        Assert.AreEqual(0, result.Length)

        let result = MonthlyStats.leaderboard_7k 1
        printfn "%A" result
        Assert.True(result.Length >= 1)

        let result = MonthlyStats.leaderboard_7k 2
        Assert.AreEqual(0, result.Length)

        MonthlyStats.save 10 user_id stats
        let result = MonthlyStats.leaderboard_4k 10
        printfn "%A" result
        Assert.AreEqual(1, result.Length)
        Assert.AreEqual(user_id, result.[0].UserId)