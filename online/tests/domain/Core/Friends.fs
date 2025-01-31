namespace Interlude.Web.Tests.Domain.Core

open NUnit.Framework

open Interlude.Web.Server.Domain.Core

module Friends =

    let OK : Result<unit, string> = Ok()

    [<Test>]
    let Basic_RoundTrip () =
        let id1 = User.create ("BasicRoundTripFriendA", 0uL) |> User.save_new
        let id2 = User.create ("BasicRoundTripFriendB", 0uL) |> User.save_new
        let id3 = User.create ("BasicRoundTripFriendC", 0uL) |> User.save_new

        Assert.AreEqual(OK, Friends.add (id1, id2))
        Assert.AreEqual(OK, Friends.add (id1, id3))
        Assert.AreEqual(OK, Friends.add (id3, id1))

        Assert.IsEmpty(Friends.get_following_ids id2)
        Assert.AreEqual(Friends.get_following_ids id1, Set.ofList [ id2; id3 ])
        Assert.IsEmpty(Friends.get_following_ids 32767)

        Assert.AreEqual(Friends.get_followers_ids id1, Set.ofList [ id3 ])
        Assert.AreEqual(Friends.get_followers_ids id3, Set.ofList [ id1 ])
        Assert.IsEmpty(Friends.get_followers_ids 32767)

    [<Test>]
    let Relations () =
        let id1 = User.create ("RelationsFriendA", 0uL) |> User.save_new
        let id2 = User.create ("RelationsFriendB", 0uL) |> User.save_new
        let id3 = User.create ("RelationsFriendC", 0uL) |> User.save_new

        Assert.AreEqual(OK, Friends.add (id1, id2))
        Assert.AreEqual(OK, Friends.add (id1, id3))
        Assert.AreEqual(OK, Friends.add (id3, id1))

        Assert.AreEqual(FriendRelation.None, Friends.relation (32767, 32767))
        Assert.AreEqual(FriendRelation.None, Friends.relation (id1, 32767))
        Assert.AreEqual(FriendRelation.None, Friends.relation (32767, id1))
        Assert.AreEqual(FriendRelation.None, Friends.relation (id1, id1))
        Assert.AreEqual(FriendRelation.Friend, Friends.relation (id1, id2))
        Assert.AreEqual(FriendRelation.FollowsYou, Friends.relation (id2, id1))
        Assert.AreEqual(FriendRelation.MutualFriend, Friends.relation (id1, id3))
        Assert.AreEqual(FriendRelation.MutualFriend, Friends.relation (id3, id1))

    [<Test>]
    let CannotFriendSelf () =
        let id1 = User.create ("CannotFriendSelf", 0uL) |> User.save_new
        Assert.AreEqual(FriendRelation.None, Friends.relation (id1, id1))

        match Friends.add(id1, id1) with
        | Error reason -> printfn "%s" reason
        | Ok() -> Assert.Fail()

        Assert.AreEqual(FriendRelation.None, Friends.relation (id1, id1))

    [<Test>]
    let Remove () =
        let id1 = User.create ("RemoveFriendA", 0uL) |> User.save_new
        let id2 = User.create ("RemoveFriendB", 0uL) |> User.save_new

        Friends.add (id1, id2) |> ignore
        Friends.remove (id1, id2)

        Assert.AreEqual(FriendRelation.None, Friends.relation (id1, id2))

    [<Test>]
    let Remove_Idempotent () =
        let id1 = User.create ("RemoveFriendIdempotentA", 0uL) |> User.save_new
        let id2 = User.create ("RemoveFriendIdempotentB", 0uL) |> User.save_new

        Friends.add (id1, id2) |> ignore
        Friends.remove (id1, id2)
        Friends.remove (id1, id2)
        Friends.remove (id1, id2)

        Friends.remove (id2, id1)
        Friends.remove (id2, id1)

        Assert.AreEqual(FriendRelation.None, Friends.relation (id1, id2))
        Assert.AreEqual(FriendRelation.None, Friends.relation (id2, id1))

    [<Test>]
    let Add_Idempotent () =
        let id1 = User.create ("AddFriendIdempotentA", 0uL) |> User.save_new
        let id2 = User.create ("AddFriendIdempotentB", 0uL) |> User.save_new

        Assert.AreEqual(OK, Friends.add (id1, id2))
        Assert.AreEqual(OK, Friends.add (id1, id2))
        Assert.AreEqual(OK, Friends.add (id1, id2))
        Assert.AreEqual(OK, Friends.add (id1, id2))

        Assert.AreEqual(FriendRelation.Friend, Friends.relation (id1, id2))
        Assert.AreEqual(Friends.get_following_ids id1, Set.ofList [ id2 ])
        Assert.AreEqual(Friends.get_followers_ids id2, Set.ofList [ id1 ])

    [<Test>]
    let List () =
        let id1 = User.create ("FriendListA", 0uL) |> User.save_new
        let id2 = User.create ("FriendListB", 0uL) |> User.save_new
        let id3 = User.create ("FriendListC", 0uL) |> User.save_new

        Assert.AreEqual(OK, Friends.add (id1, id2))
        Assert.AreEqual(OK, Friends.add (id1, id3))

        let result = Friends.friends_list id1
        Assert.AreEqual(2, result.Length)

    [<Test>]
    let Add_NonExistentUser () =
        let user_id = User.create ("AddNonExistentFriend", 0uL) |> User.save_new

        Assert.Throws(fun () -> Friends.add (user_id, 32767) |> ignore) |> printfn "%O"
        Assert.Throws(fun () -> Friends.add (32767, user_id) |> ignore) |> printfn "%O"
        Assert.Throws(fun () -> Friends.add (32767, 32768) |> ignore) |> printfn "%O"

    [<Test>]
    let Remove_NonExistentUser () =
        let user_id = User.create ("RemoveNonExistentFriend", 0uL) |> User.save_new

        Assert.Throws(fun () -> Friends.remove (user_id, 32767)) |> printfn "%O"
        Assert.Throws(fun () -> Friends.remove (32767, user_id)) |> printfn "%O"
        Assert.Throws(fun () -> Friends.remove (32767, 32768)) |> printfn "%O"

    [<Test>]
    let FriendLimit () =
        let user_id = User.create ("FriendLimitMain", 0uL) |> User.save_new

        for i = 1 to Friends.MAX_FRIENDS do
            let friend_id = User.create (sprintf "FriendLimit%i" i, 0uL) |> User.save_new
            Assert.AreEqual(OK, Friends.add(user_id, friend_id))

        let friend_id = User.create (sprintf "FriendLimitMax", 0uL) |> User.save_new
        match Friends.add(user_id, friend_id) with
        | Ok() -> Assert.Fail()
        | Error reason -> Assert.Pass(reason)