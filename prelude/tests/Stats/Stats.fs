namespace Prelude.Tests.Stats

open System
open NUnit.Framework
open Percyqaz.Common
open Prelude.Data.User.Stats

module Stats =

    [<Test>]
    let MergeKeymodePlaytimes_CaseA () =
        let original = Map.ofSeq [ 4, 3600.0; 7, 1800.0 ]
        let incoming = Map.ofSeq [ 8, 900.0; 4, 400.0 ]

        let combined = add_playtimes original incoming
        Assert.AreEqual(3, combined.Count)
        Assert.AreEqual(Some 4000.0, combined.TryFind 4)
        Assert.AreEqual(Some 1800.0, combined.TryFind 7)
        Assert.AreEqual(Some 900.0, combined.TryFind 8)

    [<Test>]
    let MergeKeymodePlaytimes_CaseB () =
        let original = Map.ofSeq [ 4, 3600.0; 7, 1800.0 ]
        let incoming = Map.empty

        let combined = add_playtimes original incoming
        Assert.AreEqual(original, combined)

        let combined2 = add_playtimes incoming original
        Assert.AreEqual(original, combined2)

    [<Test>]
    let LeaderboardMonths_FromTimestamp () =
        let start_of_2025 = DateTime(2025, 1, 1, 0, 0, 0, DateTimeKind.Utc) |> Timestamp.from_datetime

        Assert.AreEqual(0, timestamp_to_leaderboard_month (start_of_2025 - 1L))
        Assert.AreEqual(1, timestamp_to_leaderboard_month start_of_2025)
        Assert.AreEqual(1, timestamp_to_leaderboard_month (start_of_2025 + 1L))

        let may_2025 = DateTime(2025, 5, 24, 12, 34, 56, DateTimeKind.Utc) |> Timestamp.from_datetime

        Assert.AreEqual(5, timestamp_to_leaderboard_month (may_2025 - 1L))
        Assert.AreEqual(5, timestamp_to_leaderboard_month may_2025)
        Assert.AreEqual(5, timestamp_to_leaderboard_month (may_2025 + 1L))

    [<Test>]
    let LeaderboardMonths_ToTimestamp () =
        let start_of_2025 = DateTime(2025, 1, 1, 0, 0, 0, DateTimeKind.Utc) |> Timestamp.from_datetime
        Assert.AreEqual(start_of_2025, start_of_leaderboard_month 1)

        let may_2025 = DateTime(2025, 5, 24, 12, 34, 56, DateTimeKind.Utc) |> Timestamp.from_datetime
        Assert.AreEqual(5, timestamp_to_leaderboard_month (may_2025 - 1L) |> start_of_leaderboard_month |> timestamp_to_leaderboard_month)