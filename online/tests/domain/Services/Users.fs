namespace Interlude.Web.Tests.Domain.Services

open NUnit.Framework
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
        Friends.add (user_id, user2_id)
        Friends.add (user2_id, user_id)

        let replay =
            Replay2.create (user_id, Scores.CRESCENT_MOON, Scores.TIMEPLAYED, Scores.CRESCENT_MOON_REPLAY_DATA)

        let replay_id = Replay2.save_leaderboard replay

        let score =
            Score2
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

        let score_id = Score2.save score

        Users.permanently_delete_user user_id

        Assert.IsEmpty(Friends.friends_list user2_id)
        Assert.IsEmpty(Friends.friends_list user_id)
        Assert.AreEqual(None, Replay2.by_id replay_id)
        Assert.AreEqual(None, Score2.by_id score_id)
        Assert.AreEqual(None, User.by_id user_id)