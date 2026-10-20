namespace Interlude.Web.Tests.Domain.Core

open NUnit.Framework

open Interlude.Web.Server.Domain.Core

module TablesTests =

    [<Test>]
    let TableRating_RoundTrip () =
        let user_id = User.create("TableRatingRoundTrip", 0uL) |> User.save_new
        TableRating.set user_id "RoundTrip" 10.0

        match TableRating.get user_id "RoundTrip" with
        | Some(rating, _) -> Assert.AreEqual(10.0, rating)
        | None -> Assert.Fail()

    [<Test>]
    let TableRating_SetIdempotent () =
        let user_id = User.create("TableRatingSetIdempotent", 0uL) |> User.save_new
        TableRating.set user_id "Table" 10.0
        TableRating.set user_id "Table" 11.0
        TableRating.set user_id "Table" 10.0
        TableRating.set user_id "Table" 10.0

        match TableRating.get user_id "Table" with
        | Some(rating, _) -> Assert.AreEqual(10.0, rating)
        | None -> Assert.Fail()

    [<Test>]
    let TableRating_TableDoesntExist () =
        let user_id = User.create("TableRatingTableDoesntExist", 0uL) |> User.save_new
        TableRating.set user_id "Table" 10.0

        Assert.AreEqual(None, TableRating.get user_id "TableDoesntExist")

    [<Test>]
    let TableRating_UserDoesntExist () =
        let user_id = User.create("TableRatingUserDoesntExist", 0uL) |> User.save_new
        TableRating.set user_id "Table" 10.0

        Assert.AreEqual(None, TableRating.get 99999l "Table")

    [<Test>]
    let TableRating_Leaderboard () =
        let user_id1 = User.create("TableRatingLeaderboardA", 0uL) |> User.save_new
        let user_id2 = User.create("TableRatingLeaderboardB", 0uL) |> User.save_new
        let user_id3 = User.create("TableRatingLeaderboardC", 0uL) |> User.save_new
        let user_id4 = User.create("TableRatingLeaderboardD", 0uL) |> User.save_new

        TableRating.set user_id1 "TableA" 10.0
        TableRating.set user_id2 "TableB" 5.0
        TableRating.set user_id3 "TableA" 100.0
        TableRating.set user_id3 "TableB" 0.0
        TableRating.set user_id4 "TableA" 11.0

        let leaderboard_a = TableRating.leaderboard "TableA"

        Assert.AreEqual(3, leaderboard_a.Length)

        Assert.AreEqual(user_id3, leaderboard_a.[0].UserId)
        Assert.AreEqual(user_id4, leaderboard_a.[1].UserId)
        Assert.AreEqual(user_id1, leaderboard_a.[2].UserId)

        Assert.AreEqual(100.0, leaderboard_a.[0].Rating)
        Assert.AreEqual(11.0, leaderboard_a.[1].Rating)
        Assert.AreEqual(10.0, leaderboard_a.[2].Rating)

        let leaderboard_b = TableRating.leaderboard "TableB"

        Assert.AreEqual(2, leaderboard_b.Length)

        Assert.AreEqual(user_id2, leaderboard_b.[0].UserId)
        Assert.AreEqual(user_id3, leaderboard_b.[1].UserId)

        Assert.AreEqual(5.0, leaderboard_b.[0].Rating)
        Assert.AreEqual(0.0, leaderboard_b.[1].Rating)

    [<Test>]
    let TableRating_EmptyLeaderboard () =
        Assert.AreEqual([||], TableRating.leaderboard "NonExistentTable")
