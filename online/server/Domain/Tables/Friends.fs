namespace Interlude.Web.Server.Domain.New

open Percyqaz.Common
open Percyqaz.Data.Sqlite
open Prelude.Common
open Interlude.Web.Server

[<RequireQualifiedAccess>]
type FriendRelation =
    | None
    | Friend
    | MutualFriend
    | FollowsYou

module Friends =

    let internal CREATE_TABLE : NonQuery<unit> =
        { NonQuery.without_parameters() with
            SQL = """
            CREATE TABLE friends (
                UserId INTEGER PRIMARY KEY NOT NULL,
                Following TEXT NOT NULL,
                Followers TEXT NOT NULL,
                FOREIGN KEY (UserId) REFERENCES users(Id) ON DELETE CASCADE
            );
            """
        }

    let private GET_FOLLOWING_IDS : Query<int64, Set<int64>> =
        {
            SQL = "SELECT [Following] FROM friends WHERE UserId = @UserId"
            Parameters = [ "@UserId", SqliteType.Integer, 8 ]
            FillParameters = fun p id -> p.Int64 id
            Read = fun r -> r.Json JSON
        }
    let get_following_ids (user_id: int64) : Set<int64> = 
        GET_FOLLOWING_IDS.Execute user_id db |> expect |> Array.tryExactlyOne |> Option.defaultValue Set.empty
        
    let private GET_FOLLOWERS_IDS : Query<int64, Set<int64>> =
        {
            SQL = "SELECT [Followers] FROM friends WHERE UserId = @UserId"
            Parameters = [ "@UserId", SqliteType.Integer, 8 ]
            FillParameters = fun p id -> p.Int64 id
            Read = fun r -> r.Json JSON
        }
    let get_followers_ids (user_id: int64) : Set<int64> = 
        GET_FOLLOWERS_IDS.Execute user_id db |> expect |> Array.tryExactlyOne |> Option.defaultValue Set.empty

    type private UpdateFollowingFollowersModel = { UserId: int64; UserNewFollowing: Set<int64>; FriendId: int64; FriendNewFollowers: Set<int64> }
    let private UPDATE_FOLLOWING_FOLLOWERS : NonQuery<UpdateFollowingFollowersModel> =
        {
            SQL = """
            BEGIN TRANSACTION;

            INSERT INTO friends (UserId, "Following", Followers)
            VALUES (@UserId, @UserNewFollowing, '[]')
            ON CONFLICT DO UPDATE SET "Following" = excluded.Following;
            
            INSERT INTO friends (UserId, "Following", Followers)
            VALUES (@FriendId, '[]', @FriendNewFollowers)
            ON CONFLICT DO UPDATE SET Followers = excluded.Followers;

            COMMIT;
            """
            Parameters = 
                [
                    "@UserId", SqliteType.Integer, 8
                    "@UserNewFollowing", SqliteType.Text, -1
                    "@FriendId", SqliteType.Integer, 8
                    "@FriendNewFollowers", SqliteType.Text, -1
                ]
            FillParameters = (fun p m ->
                p.Int64 m.UserId
                p.Json JSON m.UserNewFollowing
                p.Int64 m.FriendId
                p.Json JSON m.FriendNewFollowers
            )
        }

    // todo: restrict max friends to 50 or so
    let add (user_id: int64, friend_id: int64) =
        if user_id = friend_id then () else

        UPDATE_FOLLOWING_FOLLOWERS.Execute 
            { 
                UserId = user_id
                UserNewFollowing = Set.add friend_id (get_following_ids user_id)
                FriendId = friend_id
                FriendNewFollowers = Set.add user_id (get_followers_ids friend_id)
            }
            db
        |> expect
        |> ignore

    let remove (user_id: int64, friend_id: int64) =
        if user_id = friend_id then () else
    
        UPDATE_FOLLOWING_FOLLOWERS.Execute 
            { 
                UserId = user_id
                UserNewFollowing = Set.remove friend_id (get_following_ids user_id)
                FriendId = friend_id
                FriendNewFollowers = Set.remove user_id (get_followers_ids friend_id)
            }
            db
        |> expect
        |> ignore

    let private GET_FOLLOWING_FOLLOWERS_IDS : Query<int64, Set<int64> * Set<int64>> =
        {
            SQL = "SELECT Following, Followers FROM friends WHERE UserId = @UserId"
            Parameters = [ "@UserId", SqliteType.Integer, 8 ]
            FillParameters = fun p id -> p.Int64 id
            Read = fun r -> r.Json JSON, r.Json JSON
        }
    let relation (user_id: int64, friend_id: int64) =
        if user_id = friend_id then FriendRelation.None else

        match GET_FOLLOWING_FOLLOWERS_IDS.Execute user_id db |> expect |> Array.tryExactlyOne with
        | Some (following, followers) ->

            let friend = Set.contains friend_id following
            let friends_with_you = Set.contains friend_id followers

            if friend && friends_with_you then FriendRelation.MutualFriend
            elif friends_with_you then FriendRelation.FollowsYou
            elif friend then FriendRelation.Friend
            else FriendRelation.None

        | None -> FriendRelation.None
        
    let friends_list (user_id: int64) : (int64 * User) array =
        get_following_ids user_id
        |> Array.ofSeq
        |> User.by_ids