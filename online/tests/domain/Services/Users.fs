namespace Interlude.Web.Tests.Domain.Services

open NUnit.Framework

open Interlude.Web.Server.Domain.Objects
open Interlude.Web.Server.Domain.Services

module Users = 

    module Scores = Interlude.Web.Tests.Domain.Objects.Scores
    
    [<Test>]
    let DeleteUser_FullCleanup () =
        let user = User.create ("DeleteUser", 999999uL)
        let user_id = User.save_new user

        let user2_id = User.create ("DeleteUserFriend", 0uL) |> User.save_new
        Friends.add (user_id, user2_id)
        Friends.add (user2_id, user_id)

        let replay = Replay.create (user_id, Scores.CRESCENT_MOON, Scores.TIMEPLAYED, Scores.CRESCENT_MOON_REPLAY_DATA)
        let replay_id = Replay.save_leaderboard "RulesetId" replay
        let score = Score.create(user_id, Scores.CRESCENT_MOON, "RulesetId", Scores.TIMEPLAYED, 1.0f, Map.empty, true, 0.98, 1, 3).WithReplay replay_id
        let score_id = Score.save score

        Users.permanently_delete_user user_id

        Assert.IsEmpty(Friends.friends_list user2_id)
        Assert.IsEmpty(Friends.friends_list user_id)
        Assert.AreEqual(None, Replay.by_id replay_id)
        Assert.AreEqual(None, Score.by_id score_id)
        Assert.AreEqual(None, User.by_id user_id)
