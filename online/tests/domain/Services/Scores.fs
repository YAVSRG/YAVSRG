namespace Interlude.Web.Tests.Domain.Services

open NUnit.Framework
open Prelude
open Percyqaz.Common
open Prelude.Gameplay.Replays
open Interlude.Web.Server.Domain.Core
open Interlude.Web.Server.Domain.Services

module Scores =

    module Scores = Interlude.Web.Tests.Domain.Core.Scores

    [<Test>]
    let DecompressUntrustedString () =
        match Replay.decompress_string_untrusted 200000.0f<ms> Scores.CRESCENT_MOON_REPLAY_STRING with
        | Ok _ -> Assert.Pass()
        | Error e -> Assert.Fail(e)

    [<Test>]
    let ScoreSubmission_HappyPath () =
        let user = User.create ("ScoreSubmissionHappyPath", 999999uL)
        let user_id = User.save_new user

        let result =
            Scores.submit (
                user_id,
                Scores.CRESCENT_MOON,
                Scores.CRESCENT_MOON_REPLAY_STRING,
                1.0f<rate>,
                Map.empty,
                Scores.TIMEPLAYED
            )
            |> Async.RunSynchronously

        printfn "%A" result

    [<Test>]
    let ScoreSubmission_FakeData () =
        let user = User.create ("ScoreSubmissionFakeData", 999999uL)
        let user_id = User.save_new user

        let result =
            Scores.submit (
                user_id,
                Scores.CRESCENT_MOON,
                "FAKEREPLAYDATAFAKEREPLAYDATA",
                1.0f<rate>,
                Map.empty,
                Scores.TIMEPLAYED
            )
            |> Async.RunSynchronously

        printfn "%A" result

    [<Test>]
    let ScoreSubmission_BadModCombo () =
        let user = User.create ("ScoreSubmissionBadModCombo", 999999uL)
        let user_id = User.save_new user

        let result =
            Scores.submit (
                user_id,
                Scores.CRESCENT_MOON,
                Scores.CRESCENT_MOON_REPLAY_STRING,
                1.0f<rate>,
                Map.ofList [ "invalid", 999 ],
                Scores.TIMEPLAYED
            )
            |> Async.RunSynchronously

        printfn "%A" result

    [<Test>]
    let LeaderboardPosition_Empty () =

        let score = Score.create (9999L, "LeaderboardPositionEmpty", Timestamp.now(), 1.0f<rate>, Map.empty, true, 0.95, 3, 2)
        let result = Scores.new_leaderboard_position score

        Assert.AreEqual(Some 1, result)

    [<Test>]
    let LeaderboardPosition_2Of3 () =

        let user1 = User.create ("LeaderboardPosition2Of3A", 999999uL)
        let user_id1 = User.save_new user1

        let user2 = User.create ("LeaderboardPosition2Of3B", 999999uL)
        let user_id2 = User.save_new user2

        Score.create (user_id1, "LeaderboardPosition2Of3", Timestamp.now(), 1.0f<rate>, Map.empty, true, 0.97, 3, 2)
        |> Score.save
        |> ignore

        Score.create (user_id2, "LeaderboardPosition2Of3", Timestamp.now(), 1.0f<rate>, Map.empty, true, 0.93, 3, 2)
        |> Score.save
        |> ignore

        let score = Score.create (9999L, "LeaderboardPosition2Of3", Timestamp.now(), 1.0f<rate>, Map.empty, true, 0.95, 3, 2)
        let result = Scores.new_leaderboard_position score

        Assert.AreEqual(Some 2, result)

    [<Test>]
    let LeaderboardPosition_Full () =

        for i = 0 to Score.LEADERBOARD_SIZE - 1 do

            let user = User.create (sprintf "LeaderboardPositionFull_%i" i, 999999uL)
            let user_id = User.save_new user

            Score.create (user_id, "LeaderboardPositionFull", Timestamp.now(), 1.0f<rate>, Map.empty, true, 0.97, 3, 2)
            |> Score.save
            |> ignore

        let score = Score.create (9999L, "LeaderboardPositionFull", Timestamp.now(), 1.0f<rate>, Map.empty, true, 0.95, 3, 2)
        let result = Scores.new_leaderboard_position score

        Assert.AreEqual(None, result)

    [<Test>]
    let LeaderboardPosition_LastPlace () =

        for i = 0 to Score.LEADERBOARD_SIZE - 2 do

            let user = User.create (sprintf "LeaderboardPositionLastPlace_%i" i, 999999uL)
            let user_id = User.save_new user

            Score.create (user_id, "LeaderboardPositionLastPlace", Timestamp.now(), 1.0f<rate>, Map.empty, true, 0.97, 3, 2)
            |> Score.save
            |> ignore

        let score = Score.create (9999L, "LeaderboardPositionLastPlace", Timestamp.now(), 1.0f<rate>, Map.empty, true, 0.95, 3, 2)
        let result = Scores.new_leaderboard_position score

        Assert.AreEqual(Some Score.LEADERBOARD_SIZE, result)

    [<Test>]
    let LeaderboardPosition_OldScoreBetter () =
        let user1 = User.create ("LeaderboardPositionOldScoreBetterA", 999999uL)
        let user_id1 = User.save_new user1

        let user2 = User.create ("LeaderboardPositionOldScoreBetterB", 999999uL)
        let user_id2 = User.save_new user2

        Score.create (user_id1, "LeaderboardPositionOldScoreBetter", Timestamp.now(), 1.0f<rate>, Map.empty, true, 0.97, 3, 2)
        |> Score.save
        |> ignore

        Score.create (user_id2, "LeaderboardPositionOldScoreBetter", Timestamp.now(), 1.0f<rate>, Map.empty, true, 0.95, 3, 2)
        |> Score.save
        |> ignore

        let score = Score.create (user_id2, "LeaderboardPositionOldScoreBetter", Timestamp.now(), 1.0f<rate>, Map.empty, true, 0.94, 3, 2)
        let result = Scores.new_leaderboard_position score

        Assert.AreEqual(None, result)

    [<Test>]
    let LeaderboardPosition_NewScoreBetter () =
        let user1 = User.create ("LeaderboardPositionNewScoreBetterA", 999999uL)
        let user_id1 = User.save_new user1

        let user2 = User.create ("LeaderboardPositionNewScoreBetterB", 999999uL)
        let user_id2 = User.save_new user2

        Score.create (user_id1, "LeaderboardPositionNewScoreBetter", Timestamp.now(), 1.0f<rate>, Map.empty, true, 0.97, 3, 2)
        |> Score.save
        |> ignore

        Score.create (user_id2, "LeaderboardPositionNewScoreBetter", Timestamp.now(), 1.0f<rate>, Map.empty, true, 0.94, 3, 2)
        |> Score.save
        |> ignore

        let score = Score.create (user_id2, "LeaderboardPositionNewScoreBetter", Timestamp.now(), 1.0f<rate>, Map.empty, true, 0.95, 3, 2)
        let result = Scores.new_leaderboard_position score

        Assert.AreEqual(Some 2, result)