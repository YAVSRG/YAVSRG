namespace Interlude.Web.Tests.Domain.Core

open NUnit.Framework

open Interlude.Web.Server.Domain.Core
open Interlude.Web.Server.Domain.Services

module Users =

    [<SetUp>]
    let Setup () = ()

    [<Test>]
    let RoundTrip_ById () =
        let user = User.create ("RoundTripById", 11111uL)
        let id = User.save_new user

        match User.by_id id with
        | Some fetched_user -> Assert.AreEqual(user, fetched_user)
        | None -> Assert.Fail()

    [<Test>]
    let NotFound_ById () =
        match User.by_id 32767 with
        | Some _ -> Assert.Fail()
        | None -> Assert.Pass()

    [<Test>]
    let RoundTrip_ByUsername_ExactCase () =
        let user = User.create ("UsernameExactCase", 22222uL)
        let id = User.save_new user

        match User.by_username "UsernameExactCase" with
        | Some(fetched_id, fetched_user) ->
            Assert.AreEqual(user, fetched_user)
            Assert.AreEqual(id, fetched_id)
        | None -> Assert.Fail()

    [<Test>]
    let RoundTrip_ByUsername_Lowercase () =
        let user = User.create ("UsernameLowercase", 33333uL)
        let id = User.save_new user

        match User.by_username "usernamelowercase" with
        | Some(fetched_id, fetched_user) ->
            Assert.AreEqual(user, fetched_user)
            Assert.AreEqual(id, fetched_id)
        | None -> Assert.Fail()

    [<Test>]
    let RoundTrip_ByUsername_Underscore () =
        User.create ("UnderXScore", 0uL) |> User.save_new |> ignore
        let user = User.create ("Under_Score", 33333uL)
        let id = User.save_new user

        match User.by_username "under_score" with
        | Some(fetched_id, fetched_user) ->
            Assert.AreEqual(user, fetched_user)
            Assert.AreEqual(id, fetched_id)
        | None -> Assert.Fail()

    [<Test>]
    let NotFound_ByUsername () =
        match User.by_username "notfound" with
        | Some _ -> Assert.Fail()
        | None -> Assert.Pass()

    [<Test>]
    let RoundTrip_ByUsername_MixedCase () =
        let user = User.create ("UsErNaMeMiXeDcAsE", 44444uL)
        let id = User.save_new user

        match User.by_username "uSeRnAmEmIxEdCaSe" with
        | Some(fetched_id, fetched_user) ->
            Assert.AreEqual(user, fetched_user)
            Assert.AreEqual(id, fetched_id)
        | None -> Assert.Fail()

    [<Test>]
    let RoundTrip_ByAuthToken () =
        let user = User.create ("AuthToken", 55555uL)
        let id = User.save_new user

        match User.by_auth_token user.AuthToken with
        | Some(fetched_id, fetched_user) ->
            Assert.AreEqual(user, fetched_user)
            Assert.AreEqual(id, fetched_id)
        | None -> Assert.Fail()

    [<Test>]
    let NotFound_ByAuthToken () =
        match User.by_auth_token "invalid_auth_token" with
        | Some _ -> Assert.Fail()
        | None -> Assert.Pass()

    [<Test>]
    let RoundTrip_ByDiscordId () =
        let user = User.create ("DiscordId", System.UInt64.MaxValue - 2uL)
        let id = User.save_new user

        match User.by_discord_id user.DiscordId with
        | Some(fetched_id, fetched_user) ->
            Assert.AreEqual(user, fetched_user)
            Assert.AreEqual(id, fetched_id)
        | None -> Assert.Fail()

    [<Test>]
    let NotFound_ByDiscordId () =
        match User.by_discord_id System.UInt64.MaxValue with
        | Some _ -> Assert.Fail()
        | None -> Assert.Pass()

    [<Test>]
    let UpdateBadges_OnNonExistentUser () =
        User.update_badges (32767, Set.ofList [ Badge.DEVELOPER; Badge.DONATOR; Badge.EARLYTESTER ])

    [<Test>]
    let UpdateBadges () =
        let user = User.create ("UpdateBadges", 66666uL)
        let id = User.save_new user
        let badges = Set.ofList [ Badge.DEVELOPER; Badge.DONATOR; Badge.EARLYTESTER ]

        User.update_badges (id, badges)

        match User.by_id id with
        | Some retrieved_user -> Assert.AreEqual({ user with Badges = badges }, retrieved_user)
        | None -> Assert.Fail()

    [<Test>]
    let UpdateColor_OnNonExistentUser () = User.update_color (32767, 0xFF00DDEE)

    [<Test>]
    let UpdateColor () =
        let user = User.create ("UpdateColor", 77777uL)
        let id = User.save_new user
        let color = 0xFF00DDEE

        User.update_color (id, color)

        match User.by_id id with
        | Some retrieved_user -> Assert.AreEqual({ user with Color = color }, retrieved_user)
        | None -> Assert.Fail()

    [<Test>]
    let UpdateLastSeen_OnNonExistentUser () = User.update_last_seen (32767)

    [<Test>]
    let UpdateLastSeen () =
        let user = User.create ("UpdateLastSeen", 88888uL)
        let id = User.save_new user

        User.update_last_seen (id)

        match User.by_id id with
        | Some retrieved_user ->
            Assert.AreEqual({ retrieved_user with LastLogin = 0L }, user)
            Assert.AreNotEqual(user.LastLogin, retrieved_user.LastLogin)
        | None -> Assert.Fail()

    [<Test>]
    let SetAuthToken_OnNonExistentUser () =
        User.set_auth_token (32767, User.generate_auth_token ())

    [<Test>]
    let SetAuthToken () =
        let user = User.create ("SetAuthToken", 99999uL)
        let id = User.save_new user
        let token = User.generate_auth_token ()

        User.set_auth_token (id, token)

        match User.by_id id with
        | Some retrieved_user -> Assert.AreEqual({ user with AuthToken = token }, retrieved_user)
        | None -> Assert.Fail()

    [<Test>]
    let Search () =
        User.create ("AAASearch1", 0uL) |> User.save_new |> ignore
        User.create ("SearchBBB2", 0uL) |> User.save_new |> ignore
        User.create ("CCCSearch", 0uL) |> User.save_new |> ignore
        User.create ("search", 0uL) |> User.save_new |> ignore
        User.create ("SEARCH_", 0uL) |> User.save_new |> ignore
        User.create ("DDDRedHerring1", 0uL) |> User.save_new |> ignore
        User.create ("RedHerring2", 0uL) |> User.save_new |> ignore

        let results = User.search_by_username "search"
        let results_uppercase = User.search_by_username "SEARCH"

        Assert.AreEqual(5, results.Length)
        Assert.AreEqual(results, results_uppercase)

    [<Test>]
    let Search_Underscore () =
        User.create ("Under__score", 0uL) |> User.save_new |> ignore
        User.create ("Underaascore", 0uL) |> User.save_new |> ignore
        User.create ("Underbbscore", 0uL) |> User.save_new |> ignore

        let results = User.search_by_username "under__scor"

        Assert.AreEqual(1, results.Length)

    [<Test>]
    let List () =
        User.create ("ListUser1", 0uL) |> User.save_new |> ignore
        User.create ("ListUser2", 0uL) |> User.save_new |> ignore
        User.create ("ListUser3", 0uL) |> User.save_new |> ignore

        let results = User.list 0

        Assert.LessOrEqual(3, results.Length)

    [<Test>]
    let List_BigPageNumber () =
        User.create ("ListBigPageUser1", 0uL) |> User.save_new |> ignore
        User.create ("ListBigPageUser2", 0uL) |> User.save_new |> ignore
        User.create ("ListBigPageUser3", 0uL) |> User.save_new |> ignore

        let results = User.list 10000

        Assert.AreEqual(0, results.Length)

    [<Test>]
    let List_NegativePage () =
        let results = User.list -1

        Assert.AreEqual(0, results.Length)

    [<Test>]
    let ByIds_Empty () =
        Assert.AreEqual(0, (User.by_ids [||]).Length)

    [<Test>]
    let ByIds_NotFound () =
        let results = User.by_ids [| 32767; 32768; 32769 |]

        Assert.AreEqual(0, results.Length)

    [<Test>]
    let ByIds () =
        let id1 = User.create ("ByIds1", 0uL) |> User.save_new
        let id2 = User.create ("ByIds2", 0uL) |> User.save_new
        let id3 = User.create ("ByIds3", 0uL) |> User.save_new

        let results = User.by_ids [| id1; id2; id3 |]

        Assert.AreEqual(3, results.Length)

    [<Test>]
    let ByIds_Duplicates () =
        let id1 = User.create ("ByIdsDuplicates1", 0uL) |> User.save_new
        let id2 = User.create ("ByIdsDuplicates2", 0uL) |> User.save_new
        User.create ("ByIdsDuplicates3", 0uL) |> User.save_new |> ignore

        let results = User.by_ids [| id1; id2; id1; 32767 |]

        Assert.AreEqual(2, results.Length)

    [<Test>]
    let Count() =
        let original_count = User.count()

        User.create ("Count1", 0uL) |> User.save_new |> ignore

        let count_after_1 = User.count()

        User.create ("Count2", 0uL) |> User.save_new |> ignore
        User.create ("Count3", 0uL) |> User.save_new |> ignore

        let count_after_3 = User.count()

        printfn "%i %i %i" original_count count_after_1 count_after_3
        Assert.GreaterOrEqual(original_count, 0)
        Assert.AreEqual(original_count + 1L, count_after_1)
        Assert.AreEqual(original_count + 3L, count_after_3)

    [<Test>]
    let Rename() =
        let user_1 = User.create("RenameMe1", 0uL) |> User.save_new
        let user_2 = User.create("RenameMe2", 0uL) |> User.save_new

        match Users.Auth.rename "RenameMe1" "RenameMe2" with
        | Error reason -> printfn "%s" reason
        | Ok() -> Assert.Fail("This rename should fail (1 -> 2)")

        match Users.Auth.rename "RenameMe1" "RenameMe3" with
        | Error reason -> Assert.Fail(reason)
        | Ok() -> ()

        match Users.Auth.rename "RenameMe2" "RenameMe1" with
        | Error reason -> Assert.Fail(reason)
        | Ok() -> ()

        match Users.Auth.rename "RenameMe2" "RenameMe1" with
        | Error reason -> printfn "%s" reason
        | Ok() -> Assert.Fail("This rename should fail (2 -> 1)")

        match User.by_id user_1 with
        | Some user -> Assert.AreEqual("RenameMe3", user.Username)
        | None -> Assert.Fail()

        match User.by_id user_2 with
        | Some user -> Assert.AreEqual("RenameMe1", user.Username)
        | None -> Assert.Fail()