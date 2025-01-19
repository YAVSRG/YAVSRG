namespace Interlude.Web.Tests.Domain.Core

open NUnit.Framework
open Prelude
open Prelude.Gameplay.Replays
open Interlude.Web.Server.Domain.Core

module Scores =

    let CRESCENT_MOON_REPLAY_STRING =
        "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"

    let CRESCENT_MOON_REPLAY_DATA = Replay.decompress_string CRESCENT_MOON_REPLAY_STRING

    let CRESCENT_MOON =
        "1467CD6DEB4A3B87FA58FAB4F2398BE9AD7B0017031C511C549D3EF28FFB58D3"

    let SCJ4 = "SAE1C74D1"
    let TIMEPLAYED = 1705685404000L

    [<Test>]
    let RecentScore_RoundTrip () =
        let user_id = User.create ("RecentScoreRoundTrip", 0uL) |> User.save_new

        let score =
            Score.create (user_id, CRESCENT_MOON, TIMEPLAYED, 1.0f<rate>, Map.empty, true, 0.98, 1, 3)

        let score_id = Score.save score

        let results = Score.get_user_recent user_id
        Assert.AreEqual(1, results.Length)
        Assert.AreEqual(score_id, results.[0].Id)
        Assert.AreEqual(score.ChartId, results.[0].ChartId)
        Assert.AreEqual(score.TimePlayed, results.[0].TimePlayed)
        Assert.AreEqual(score.Rate, results.[0].Rate)
        Assert.AreEqual(score.Mods, results.[0].Mods)
        Assert.AreEqual(score.Accuracy, results.[0].Accuracy)
        Assert.AreEqual(score.Grade, results.[0].Grade)
        Assert.AreEqual(score.Lamp, results.[0].Lamp)

    [<Test>]
    let Leaderboard_RoundTripWithReplays () =
        Score.wipe_leaderboard CRESCENT_MOON |> printfn "Clearing %i scores + replays to set up test"

        let user1_id = User.create ("LeaderboardRoundTripWithReplayA", 0uL) |> User.save_new
        let user2_id = User.create ("LeaderboardRoundTripWithReplayB", 0uL) |> User.save_new

        let replayA =
            Replay.create (user1_id, CRESCENT_MOON, TIMEPLAYED, CRESCENT_MOON_REPLAY_DATA)

        let replayA_id = Replay.save_leaderboard replayA

        let scoreA =
            Score
                .create(user1_id, CRESCENT_MOON, TIMEPLAYED, 1.0f<rate>, Map.empty, true, 0.98, 1, 3)
                .WithReplay
                replayA_id

        let _ = Score.save scoreA

        let replayB =
            Replay.create (user2_id, CRESCENT_MOON, TIMEPLAYED + 1L, CRESCENT_MOON_REPLAY_DATA)

        let replayB_id = Replay.save_leaderboard replayB

        let scoreB =
            Score
                .create(user2_id, CRESCENT_MOON, TIMEPLAYED, 1.1f<rate>, Map.empty, true, 0.99, 0, 2)
                .WithReplay
                replayB_id

        let _ = Score.save scoreB

        let results = Score.get_leaderboard CRESCENT_MOON

        Assert.AreEqual(2, results.Length)

        Assert.AreEqual(user2_id, results.[0].UserId)
        Assert.AreEqual(Some replayB_id, results.[0].ReplayId)
        Assert.AreEqual(scoreB.Accuracy, results.[0].Accuracy)
        Assert.AreEqual(scoreB.Grade, results.[0].Grade)
        Assert.AreEqual(scoreB.Lamp, results.[0].Lamp)
        Assert.AreEqual(scoreB.Rate, results.[0].Rate)
        Assert.AreEqual(scoreB.Mods, results.[0].Mods)
        Assert.AreEqual(scoreB.TimePlayed, results.[0].TimePlayed)

        Assert.AreEqual(user1_id, results.[1].UserId)
        Assert.AreEqual(Some replayA_id, results.[1].ReplayId)
        Assert.AreEqual(scoreA.Accuracy, results.[1].Accuracy)
        Assert.AreEqual(scoreA.Grade, results.[1].Grade)
        Assert.AreEqual(scoreA.Lamp, results.[1].Lamp)
        Assert.AreEqual(scoreA.Rate, results.[1].Rate)
        Assert.AreEqual(scoreA.Mods, results.[1].Mods)
        Assert.AreEqual(scoreA.TimePlayed, results.[1].TimePlayed)

    [<Test>]
    let Leaderboard_RoundTripMultipleScoresWorstFirst () =
        Score.wipe_leaderboard CRESCENT_MOON |> printfn "Clearing %i scores + replays to set up test"

        let user_id =
            User.create ("LeaderboardRoundTripMultipleScoresWorstFirst", 0uL)
            |> User.save_new

        let replayA =
            Replay.create (user_id, CRESCENT_MOON, TIMEPLAYED, CRESCENT_MOON_REPLAY_DATA)

        let replayA_id = Replay.save_leaderboard replayA

        let scoreA =
            Score
                .create(user_id, CRESCENT_MOON, TIMEPLAYED, 1.0f<rate>, Map.empty, true, 0.98, 1, 3)
                .WithReplay
                replayA_id

        let _ = Score.save scoreA |> Score.by_id |> printfn "%A"

        let replayB =
            Replay.create (user_id, CRESCENT_MOON, TIMEPLAYED + 1L, CRESCENT_MOON_REPLAY_DATA)

        let replayB_id = Replay.save_leaderboard replayB

        let scoreB =
            Score
                .create(user_id, CRESCENT_MOON, TIMEPLAYED + 1L, 1.1f<rate>, Map.empty, true, 0.99, 0, 2)
                .WithReplay
                replayB_id

        let _ = Score.save scoreB |> Score.by_id |> printfn "%A"

        let results = Score.get_leaderboard CRESCENT_MOON

        printfn "%A" results

        Assert.AreEqual(1, results.Length)

        Assert.AreEqual(user_id, results.[0].UserId)
        Assert.AreEqual(scoreB.Accuracy, results.[0].Accuracy)
        Assert.AreEqual(scoreB.Grade, results.[0].Grade)
        Assert.AreEqual(scoreB.Lamp, results.[0].Lamp)
        Assert.AreEqual(scoreB.Rate, results.[0].Rate)
        Assert.AreEqual(scoreB.Mods, results.[0].Mods)
        Assert.AreEqual(scoreB.TimePlayed, results.[0].TimePlayed)
        Assert.AreEqual(Some replayB_id, results.[0].ReplayId)

    [<Test>]
    let Leaderboard_RoundTripMultipleScoresBestFirst () =
        Score.wipe_leaderboard CRESCENT_MOON |> printfn "Clearing %i scores + replays to set up test"

        let user_id =
            User.create ("LeaderboardRoundTripMultipleScoresBestFirst", 0uL)
            |> User.save_new

        let replayB =
            Replay.create (user_id, CRESCENT_MOON, TIMEPLAYED + 1L, CRESCENT_MOON_REPLAY_DATA)

        let replayB_id = Replay.save_leaderboard replayB

        let scoreB =
            Score
                .create(user_id, CRESCENT_MOON, TIMEPLAYED + 1L, 1.1f<rate>, Map.empty, true, 0.99, 0, 2)
                .WithReplay
                replayB_id

        let _ = Score.save scoreB

        let scoreA =
            Score.create (user_id, CRESCENT_MOON, TIMEPLAYED, 1.0f<rate>, Map.empty, true, 0.98, 1, 3)

        let _ = Score.save scoreA

        let results = Score.get_leaderboard CRESCENT_MOON

        printfn "%A" results

        Assert.AreEqual(1, results.Length)

        Assert.AreEqual(user_id, results.[0].UserId)
        Assert.AreEqual(scoreB.Accuracy, results.[0].Accuracy)
        Assert.AreEqual(scoreB.Grade, results.[0].Grade)
        Assert.AreEqual(scoreB.Lamp, results.[0].Lamp)
        Assert.AreEqual(scoreB.Rate, results.[0].Rate)
        Assert.AreEqual(scoreB.Mods, results.[0].Mods)
        Assert.AreEqual(scoreB.TimePlayed, results.[0].TimePlayed)
        Assert.AreEqual(Some replayB_id, results.[0].ReplayId)

    [<Test>]
    let LeaderboardScore_RoundTripMultipleScoresWorstFirst () =
        let user_id =
            User.create ("LeaderboardScoreRoundTripMultipleScoresWorstFirst", 0uL)
            |> User.save_new

        let replayA =
            Replay.create (user_id, CRESCENT_MOON, TIMEPLAYED, CRESCENT_MOON_REPLAY_DATA)

        let replayA_id = Replay.save_leaderboard replayA

        let scoreA =
            Score
                .create(user_id, CRESCENT_MOON, TIMEPLAYED, 1.0f<rate>, Map.empty, true, 0.98, 1, 3)
                .WithReplay
                replayA_id

        let _ = Score.save scoreA

        let replayB =
            Replay.create (user_id, CRESCENT_MOON, TIMEPLAYED + 1L, CRESCENT_MOON_REPLAY_DATA)

        let replayB_id = Replay.save_leaderboard replayB

        let scoreB =
            Score
                .create(user_id, CRESCENT_MOON, TIMEPLAYED + 1L, 1.1f<rate>, Map.empty, true, 0.99, 0, 2)
                .WithReplay
                replayB_id

        let _ = Score.save scoreB

        match Score.get_user_leaderboard_score user_id CRESCENT_MOON with
        | None -> Assert.Fail()
        | Some result ->
            Assert.AreEqual(scoreB.Accuracy, result.Accuracy)
            Assert.AreEqual(scoreB.Grade, result.Grade)
            Assert.AreEqual(scoreB.Lamp, result.Lamp)
            Assert.AreEqual(scoreB.Rate, result.Rate)
            Assert.AreEqual(scoreB.Mods, result.Mods)
            Assert.AreEqual(scoreB.TimePlayed, result.TimePlayed)
            Assert.AreEqual(Some replayB_id, result.ReplayId)

    [<Test>]
    let LeaderboardScore_RoundTripMultipleScoresBestFirst () =
        let user_id =
            User.create ("LeaderboardScoreRoundTripMultipleScoresBestFirst", 0uL)
            |> User.save_new

        let replayB =
            Replay.create (user_id, CRESCENT_MOON, TIMEPLAYED + 1L, CRESCENT_MOON_REPLAY_DATA)

        let replayB_id = Replay.save_leaderboard replayB

        let scoreB =
            Score
                .create(user_id, CRESCENT_MOON, TIMEPLAYED + 1L, 1.1f<rate>, Map.empty, true, 0.99, 0, 2)
                .WithReplay
                replayB_id

        let _ = Score.save scoreB

        let scoreA =
            Score.create (user_id, CRESCENT_MOON, TIMEPLAYED, 1.0f<rate>, Map.empty, true, 0.98, 1, 3)

        let _ = Score.save scoreA

        match Score.get_user_leaderboard_score user_id CRESCENT_MOON with
        | None -> Assert.Fail()
        | Some result ->
            Assert.AreEqual(scoreB.Accuracy, result.Accuracy)
            Assert.AreEqual(scoreB.Grade, result.Grade)
            Assert.AreEqual(scoreB.Lamp, result.Lamp)
            Assert.AreEqual(scoreB.Rate, result.Rate)
            Assert.AreEqual(scoreB.Mods, result.Mods)
            Assert.AreEqual(scoreB.TimePlayed, result.TimePlayed)
            Assert.AreEqual(Some replayB_id, result.ReplayId)

    [<Test>]
    let RoundTrips_ById () =
        let user_id = User.create ("RoundTripsById", 0uL) |> User.save_new

        let replayA =
            Replay.create (user_id, CRESCENT_MOON, TIMEPLAYED, CRESCENT_MOON_REPLAY_DATA)

        let replayA_id = Replay.save_leaderboard replayA

        let scoreA =
            Score
                .create(user_id, CRESCENT_MOON, TIMEPLAYED, 1.0f<rate>, Map.empty, true, 0.98, 1, 3)
                .WithReplay
                replayA_id

        let scoreA_id = Score.save scoreA

        let replayB =
            Replay.create (user_id, CRESCENT_MOON, TIMEPLAYED + 1L, CRESCENT_MOON_REPLAY_DATA)

        let replayB_id = Replay.save_leaderboard replayB

        let scoreB =
            Score
                .create(user_id, CRESCENT_MOON, TIMEPLAYED + 1L, 1.1f<rate>, Map.empty, true, 0.99, 0, 2)
                .WithReplay
                replayB_id

        let scoreB_id = Score.save scoreB

        match Score.by_id scoreA_id with
        | Some(retrieved_score, None) ->
            Assert.AreEqual(retrieved_score.UserId, scoreA.UserId)
            Assert.AreEqual(retrieved_score.ChartId, scoreA.ChartId)
            Assert.AreEqual(retrieved_score.TimePlayed, scoreA.TimePlayed)
            Assert.AreEqual(retrieved_score.Rate, scoreA.Rate)
            Assert.AreEqual(retrieved_score.Mods, scoreA.Mods)
            Assert.AreEqual(retrieved_score.Accuracy, scoreA.Accuracy)
            Assert.AreEqual(retrieved_score.Grade, scoreA.Grade)
            Assert.AreEqual(retrieved_score.Lamp, scoreA.Lamp)
        | _ -> Assert.Fail()

        match Score.by_id scoreB_id with
        | Some(retrieved_score, Some retrieved_replay) ->
            Assert.AreEqual(retrieved_score.UserId, scoreB.UserId)
            Assert.AreEqual(retrieved_score.ChartId, scoreB.ChartId)
            Assert.AreEqual(retrieved_score.TimePlayed, scoreB.TimePlayed)
            Assert.AreEqual(retrieved_score.Rate, scoreB.Rate)
            Assert.AreEqual(retrieved_score.Mods, scoreB.Mods)
            Assert.AreEqual(retrieved_score.Accuracy, scoreB.Accuracy)
            Assert.AreEqual(retrieved_score.Grade, scoreB.Grade)
            Assert.AreEqual(retrieved_score.Lamp, scoreB.Lamp)

            Assert.AreEqual(retrieved_replay.Data, replayB.Data)
        | _ -> Assert.Fail()

    [<Test>]
    let AggregateGrades () =
        let user_id = User.create ("AggregateGrades", 0uL) |> User.save_new

        let mutable time = TIMEPLAYED

        let save_score_and_replay (chart_id: string) (grade: int) =
            let replay = Replay.create (user_id, chart_id, time, CRESCENT_MOON_REPLAY_DATA)
            let replay_id = Replay.save_leaderboard replay

            let score =
                Score
                    .create(
                        user_id,
                        chart_id,
                        time,
                        1.05f<rate>,
                        Map.empty,
                        true,
                        0.99 - 0.01 * float grade,
                        grade,
                        3
                    )
                    .WithReplay
                    replay_id

            Score.save score |> ignore
            time <- time + 1000L

        save_score_and_replay "chart1" 1
        save_score_and_replay "chart1" 3
        save_score_and_replay "chart1" 2
        save_score_and_replay "chart2" 4
        save_score_and_replay "chart2" 2
        save_score_and_replay "chart2" 2
        save_score_and_replay "chart3" 5
        save_score_and_replay "chart3" 5
        save_score_and_replay "chart3" 5
        save_score_and_replay "chart4" 0

        let results = Score.aggregate_user_ranked_grades user_id
        let result_map = Map.ofArray results
        printfn "%A" result_map

        Assert.AreEqual(1, result_map.["chart1"])
        Assert.AreEqual(2, result_map.["chart2"])
        Assert.AreEqual(5, result_map.["chart3"])
        Assert.AreEqual(0, result_map.["chart4"])

    [<Test>]
    let AggregateScores () =
        let user_id = User.create ("AggregateScores", 0uL) |> User.save_new

        let mutable time = TIMEPLAYED

        let save_score_and_replay (chart_id: string) (grade: int) =
            let replay = Replay.create (user_id, chart_id, time, CRESCENT_MOON_REPLAY_DATA)
            let replay_id = Replay.save_leaderboard replay

            let score =
                Score
                    .create(
                        user_id,
                        chart_id,
                        time,
                        1.05f<rate>,
                        Map.empty,
                        true,
                        0.99 - 0.01 * float grade,
                        grade,
                        3
                    )
                    .WithReplay
                    replay_id

            Score.save score |> ignore
            time <- time + 1000L

        save_score_and_replay "chart1" 1
        save_score_and_replay "chart1" 3
        save_score_and_replay "chart1" 2
        save_score_and_replay "chart2" 4
        save_score_and_replay "chart2" 2
        save_score_and_replay "chart2" 2
        save_score_and_replay "chart3" 5
        save_score_and_replay "chart3" 5
        save_score_and_replay "chart3" 5
        save_score_and_replay "chart4" 0

        let results = Score.aggregate_user_ranked_scores user_id
        let result_map = Map.ofArray results
        printfn "%A" result_map

        Assert.AreEqual(0.98, result_map.["chart1"])
        Assert.AreEqual(0.97, result_map.["chart2"])
        Assert.AreEqual(0.94, result_map.["chart3"])
        Assert.AreEqual(0.99, result_map.["chart4"])

// todo: test mixing in some unranked scores too