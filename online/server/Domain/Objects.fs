namespace Interlude.Web.Server.Domain

open System
open StackExchange.Redis
open NRedisStack.Search
open NRedisStack.Search.Literals.Enums
open Percyqaz.Common
open Interlude.Web.Server.Domain.Redis

type Badge = string
module Badge =

    let DEVELOPER = "developer"
    let DONATOR = "donator"
    let MODERATOR = "moderator"
    let EARLYTESTER = "early-tester"

    let badge_color(badge: Badge) : int32 list =
        match badge with
        | _ when badge = EARLYTESTER -> [0xFF_66ff6e]
        | _ when badge = MODERATOR -> [0xFF_66c2ff]
        | _ when badge = DEVELOPER -> [0xFF_ff7559]
        | _ when badge = DONATOR -> [0xFF_ff8cdd; 0xFF_ffd36e]
        | _ -> []

    let DEFAULT_COLOR = 0xFF_cecfd9

type User =
    {
        Username: string
        DiscordId: uint64
        DateSignedUp: int64
        LastLogin: int64
        AuthToken: string
        Badges: Set<Badge>
        Color: int32 option
    }

module User =

    let id (key: string) = key.Substring(5) |> int64
    let key (id: int64) = RedisKey("user:" + id.ToString())
    let escape (query: string) = 
        let regex = System.Text.RegularExpressions.Regex("[^a-zA-Z0-9'\\-_\\s]")
        regex.Replace(query, "").Replace("'", "\\'").Replace("-", "\\-").Trim()

    let create(username, discord_id) =
        { 
            Username = username
            DiscordId = discord_id
            DateSignedUp = DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()
            LastLogin = 0L
            AuthToken = Guid.NewGuid().ToString("N")
            Badges = Set.empty
            Color = None
        }

    let save(id, user: User) =
        json.Set(key id, "$", user) |> ignore

    let save_new(user: User) : int64 =
        let new_id = db.StringIncrement("count:users", 1L)
        save(new_id, user)
        new_id

    let by_discord_id(discord_id: uint64) =
        let results = ft.Search("idx:users", Query(sprintf "@discord_id:[%i %i]" discord_id discord_id)).Documents
        Seq.tryExactlyOne results
        |> Option.map (fun d -> id d.Id, Text.Json.JsonSerializer.Deserialize<User>(d.Item "json"))

    let by_id(id: int64) =
        let result = json.Get(key id, [|"$"|])
        if result.IsNull then None 
        else 
            let s : string = RedisResult.op_Explicit result
            Some <| Text.Json.JsonSerializer.Deserialize<User>(s)

    let by_ids(ids: int64 array) =
        if ids.Length = 0 then [||] else

        json.MGet(ids |> Array.map key, "$")
        |> Array.map (fun result -> 
            if result.IsNull then None 
            else 
                let s : string = RedisResult.op_Explicit result
                Some <| Text.Json.JsonSerializer.Deserialize<User array>(s).[0]
            )

    let by_auth_token(token: string) =
        let token = escape token
        if token = "" then None else

        let results = ft.Search("idx:users", Query(sprintf "@auth_token:{%s}" token)).Documents
        Seq.tryExactlyOne results
        |> Option.map (fun d -> id d.Id, Text.Json.JsonSerializer.Deserialize<User>(d.Item "json"))
        
    let by_username(username: string) =
        let username = escape username
        if username = "" then None else

        let results = ft.Search("idx:users", Query(sprintf "@username:{%s}" username)).Documents
        Seq.tryExactlyOne results
        |> Option.map (fun d -> id d.Id, Text.Json.JsonSerializer.Deserialize<User>(d.Item "json"))

    // todo: a pagination system when there are >100 users
    let all() =
        ft.Search("idx:users", Query("*").Limit(0, 100)).Documents
        |> Seq.map (fun d -> id d.Id, Text.Json.JsonSerializer.Deserialize<User>(d.Item "json"))

module Friends =
    
    let key (id: int64) = RedisKey("friends:" + id.ToString())

    let get_id_list(userId: int64) : int64 array =
        db.SetScan(key userId)
        |> Seq.map (fun v ->
            let mutable r = 0L
            let _ = v.TryParse(&r)
            r)
        |> Array.ofSeq

    let add_friend(userId: int64, friendId: int64) =
        if userId <> friendId then db.SetAdd(key userId, friendId) |> ignore
    let remove_friend(userId: int64, friendId: int64) = db.SetRemove(key userId, friendId) |> ignore
    let has_friend(userId: int64, friendId: int64) = db.SetContains(key userId, friendId)

    let friends_list(userId: int64) = User.by_ids (get_id_list userId)

open Prelude.Gameplay

type Score =
    {
        Replay: string
        Rate: float32
        Mods: Mods.ModState
        Timestamp: int64
        UploadTimestamp: int64
    }

module Score =

    let key (userId: int64) (hash: string) = RedisKey(sprintf "score:%i:%s" userId hash)

    let create (replay: string, rate: float32, mods: Mods.ModState, timestamp: DateTime) =
        let rate = MathF.Round(rate, 2)
        if rate < 1.0f then failwith "This score is unranked because it was played at a lower rate than 1.0x"
        match Mods.check mods with
        | Ok Mods.ModStatus.Ranked ->
            {
                Replay = replay
                Rate = rate
                Mods = mods
                Timestamp = (DateTimeOffset.op_Implicit timestamp).ToUnixTimeMilliseconds()
                UploadTimestamp = DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()
            }
        | Ok _ -> failwith "This score is unranked because of mods, cannot be stored online"
        | Error reason -> failwithf "Mods are invalid: %s" reason

    let save (userId: int64) (hash: string) (score: Score) =
        json.Set(key userId hash, "$", score) |> ignore

    let get (userId: int64) (hash: string) =
        let result = json.Get(key userId hash, [|"$"|])
        if result.IsNull then None 
        else 
            let s : string = RedisResult.op_Explicit result
            Some <| Text.Json.JsonSerializer.Deserialize<Score>(s)

    let get_chart_scores (userIds: int64 array) (hash: string) =
        if userIds.Length = 0 then [||] else

        json.MGet(userIds |> Array.map (fun id -> key id hash), "$")
        |> Array.map (fun result -> 
            if result.IsNull then None 
            else 
                let s : string = RedisResult.op_Explicit result
                Some <| Text.Json.JsonSerializer.Deserialize<Score array>(s).[0]
            )

module Leaderboard =

    let private key (hash: string) (ruleset: string) = RedisKey(sprintf "leaderboard:%s:%s" hash ruleset)
    let private existence_key (hash: string) = RedisKey(sprintf "leaderboards:%s" hash)

    let add_score (userId: int64) (score: float) (hash: string) (ruleset: string) =
        db.SortedSetAdd(key hash ruleset, userId, score) |> ignore

    let get_top_20_ids (hash: string) (ruleset: string) =
        db.SortedSetRangeByRankWithScores(key hash ruleset, 0, 20, Order.Descending)
        |> Array.map (fun v ->
            let mutable r = 0L
            let _ = v.Element.TryParse(&r)
            r, v.Score)

    let get_top_20_info (hash: string) (ruleset: string) =
        let userIds =
            get_top_20_ids hash ruleset
            |> Array.map fst
        Array.zip
            (User.by_ids userIds |> Array.map (Option.map (fun x -> x.Username)))
            (Score.get_chart_scores userIds hash)

    let position (userId: int64) (hash: string) (ruleset: string) =
        let result = db.SortedSetRank(key hash ruleset, userId, Order.Descending)
        if result.HasValue then Some result.Value else None

    let exists (hash: string) (ruleset: string) =
        db.SetContains(existence_key hash, ruleset)

    let create (hash: string) (ruleset: string) =
        db.SetAdd(existence_key (hash.ToUpper()), ruleset) |> ignore

    let rulesets_by_hash (hash: string) =
        db.SetScan(existence_key hash)
        |> Seq.map (fun v -> v.ToString())
        |> Array.ofSeq

module Aggregate =
    
    let delete_user(userId: int64) =
        match User.by_id userId with
        | Some user ->
            Logging.Info(sprintf "Deleting user with id %i, discord id %i, username '%s'" userId user.DiscordId user.Username)

            let server = redis.GetServers().[0]
            let leaderboard_keys = server.Keys(pattern = RedisValue("leaderboard:*:*")) |> Array.ofSeq
            for k in leaderboard_keys do db.SortedSetRemove(k, userId) |> ignore
            Logging.Info(sprintf "Removed %s from all leaderboards (%i)" user.Username leaderboard_keys.Length)

            let score_keys = server.Keys(pattern = RedisValue(sprintf "scores:%i:*" userId)) |> Array.ofSeq
            for k in score_keys do json.Del(k) |> ignore
            Logging.Info(sprintf "Removed %s's scores (%i)" user.Username score_keys.Length)

            json.Del(Friends.key userId) |> ignore
            Logging.Info(sprintf "Removed %s's friend list" user.Username)

            json.Del(User.key userId) |> ignore
            Logging.Info(sprintf "Deleting %s complete" user.Username)

        | None -> failwithf "No such user with id %i" userId

module Migrations =

    let run() =
        Logging.Debug("Performing any necessary migrations...")

        let migration = 
            match db.StringGet("migration") with
            | v when v.IsNull -> 0L
            | v -> 
                let _, (value: int64) = v.TryParse()
                value

        if migration < 1L then
            Logging.Debug("Performing migration 1")

            ft.Create(
                "idx:users",
                FTCreateParams()
                    .On(IndexDataType.JSON)
                    .Prefix("user:"),
                Schema()
                    .AddTagField(new FieldName("$.Username", "username"), true)
                    .AddNumericField(new FieldName("$.DiscordId", "discord_id"), false)
                    .AddNumericField(FieldName("$.DateSignedUp", "date_signed_up"), true)
                    .AddNumericField(FieldName("$.LastLogin", "last_login"), true)
                    .AddTagField(FieldName("$.AuthToken", "auth_token"), false)
            ) |> ignore

            db.StringIncrement("migration", 1L) |> ignore
            Logging.Debug("Migration 1 OK")