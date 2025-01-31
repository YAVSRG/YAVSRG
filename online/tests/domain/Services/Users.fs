namespace Interlude.Web.Tests.Domain.Services

open NUnit.Framework
open Percyqaz.Common
open Prelude
open Interlude.Web.Server.Domain.Core
open Interlude.Web.Server.Domain.Services

module Users =

    module Scores = Interlude.Web.Tests.Domain.Core.Scores

    [<Test>]
    let DeleteUser_FullCleanup () =
        let user = User.create ("DeleteUser", 999999uL)
        let user_id = User.save_new user

        let user2_id = User.create ("DeleteUserFriend", 0uL) |> User.save_new
        Friends.add(user_id, user2_id) |> ignore
        Friends.add(user2_id, user_id) |> ignore

        let replay =
            Replay.create (user_id, Scores.CRESCENT_MOON, Scores.TIMEPLAYED, Scores.CRESCENT_MOON_REPLAY_DATA)

        let replay_id = Replay.save_leaderboard replay

        let score =
            Score
                .create(
                    user_id,
                    Scores.CRESCENT_MOON,
                    Scores.TIMEPLAYED,
                    1.0f<rate>,
                    Map.empty,
                    true,
                    0.98,
                    1,
                    3
                )
                .WithReplay
                replay_id

        let score_id = Score.save score

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

        Users.permanently_delete_user user_id

        Assert.IsEmpty(Friends.friends_list user2_id)
        Assert.IsEmpty(Friends.friends_list user_id)
        Assert.AreEqual(None, Replay.by_id replay_id)
        Assert.AreEqual(None, Score.by_id score_id)
        Assert.AreEqual(None, User.by_id user_id)
        Assert.AreEqual(None, MonthlyStats.get 1 user_id)