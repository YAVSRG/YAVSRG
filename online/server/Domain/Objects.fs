namespace Interlude.Web.Server.Domain

open System
open StackExchange.Redis
open NRedisStack.Search
open NRedisStack.Search.Aggregation
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

    let list(page: int) =
        ft.Search("idx:users", Query("*").SetSortBy("date_signed_up", true).Limit(page * 15, 15)).Documents
        |> Seq.map (fun d -> id d.Id, Text.Json.JsonSerializer.Deserialize<User>(d.Item "json"))
        |> Array.ofSeq

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
        UserId: int64
        ChartId: string
        RulesetId: string
        Score: float
        Grade: int
        Lamp: int
        Rate: float32
        Mods: Mods.ModState
        Timestamp: int64
    }

module Score =
    
    let SHORT_TERM_RULESET_LIST = [|"SC(J4)548E5A"|]

    let private key (id: int64) = RedisKey("score:" + id.ToString())

    let private save(id, score: Score) =
        json.Set(key id, "$", score) |> ignore
    
    let save_new(score: Score) : int64 =
        let new_id = db.StringIncrement("count:scores", 1L)
        save(new_id, score)
        new_id

    let get_recent (userId: int64) =
        let results = ft.Search("idx:scores", Query(sprintf "@user_id:[%i %i]" userId userId).SetSortBy("timestamp", false).Limit(0, 10)).Documents
        results
        |> Seq.map (fun d -> Text.Json.JsonSerializer.Deserialize<Score>(d.Item "json"))
        |> Array.ofSeq

    let exists (userId: int64) (timestamp: int64) =
        ft.Search("idx:scores", Query(sprintf "@user_id:[%i %i] @timestamp:[%i %i]" userId userId timestamp timestamp)).Documents
        |> Seq.isEmpty
        |> not

    let get_best_score (userId: int64) (ruleset_id: string) (rate: float32) (hash: string) : Score option =
        let results = ft.Search("idx:scores", 
            Query(
                sprintf "@user_id:[%i %i] @rate:[%f 2.0] @hash:{%s} @ruleset_id:{%s}" 
                    userId
                    userId
                    rate
                    hash
                    (ruleset_id.Replace(")", "\\)").Replace("(", "\\("))
             ).SetSortBy("score", false).Limit(0, 1)).Documents
        results
        |> Seq.tryHead
        |> Option.map (fun d -> Text.Json.JsonSerializer.Deserialize<Score>(d.Item "json"))

    // ft.aggregate idx:scores "@user_id:[4 4]" verbatim groupby 1 @hash reduce max 1 grade as best_grade
    // todo: filter to table hashes when there are more scores for non-table than table (soon)

    let aggregate_table_grades (userId: int64) (ruleset_id: string) (rate: float32) =
        let results = Collections.Generic.Dictionary<string, int>()
        ft.Aggregate("idx:scores", 
            AggregationRequest(
                sprintf "@user_id:[%i %i] @rate:[%f 2.0] @ruleset_id:{%s}" 
                    userId
                    userId
                    rate
                    (ruleset_id.Replace(")", "\\)").Replace("(", "\\("))
            ).Verbatim().GroupBy("@hash", Reducers.Max("grade").As("best_grade"))).GetResults()
        |> Seq.iter (fun result -> 
            let mutable g = 0
            result.["best_grade"].TryParse(&g) |> ignore
            results.[result.["hash"].ToString()] <- g)
        results

module Leaderboard =

    type Replay =
        {
            Replay: string
            Rate: float32
            Mods: Mods.ModState
            Timestamp: int64
            UploadTimestamp: int64
        }

    module Replay =

        let key (hash: string) (ruleset: string) (userId: int64) = RedisKey(sprintf "leaderboard.score:%s:%s:%i" hash ruleset userId)

        let create (replay: string, rate: float32, mods: Mods.ModState, timestamp: DateTime) =
            let rate = MathF.Round(rate, 2)
            try
                if rate < 1.0f then failwith "This score is unranked because it was played at a lower rate than 1.0x"
                match Mods.check mods with
                | Ok Mods.ModStatus.Ranked ->
                    Ok {
                        Replay = replay
                        Rate = rate
                        Mods = mods
                        Timestamp = (DateTimeOffset.op_Implicit timestamp).ToUnixTimeMilliseconds()
                        UploadTimestamp = DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()
                    }
                | Ok _ -> failwith "This score is unranked because of mods, cannot be stored online"
                | Error reason -> failwithf "Mods are invalid: %s" reason
            with err -> Error err.Message

        let save (hash: string) (ruleset: string) (userId: int64) (score: Replay) =
            json.Set(key hash ruleset userId, "$", score) |> ignore

        let get (hash: string) (ruleset: string) (userId: int64) =
            let result = json.Get(key hash ruleset userId, [|"$"|])
            if result.IsNull then None 
            else 
                let s : string = RedisResult.op_Explicit result
                Some <| Text.Json.JsonSerializer.Deserialize<Replay>(s)

        let get_chart_scores (hash: string) (ruleset: string) (userIds: int64 array) =
            if userIds.Length = 0 then [||] else

            json.MGet(userIds |> Array.map (fun id -> key hash ruleset id), "$")
            |> Array.map (fun result -> 
                if result.IsNull then None 
                else 
                    let s : string = RedisResult.op_Explicit result
                    Some <| Text.Json.JsonSerializer.Deserialize<Replay array>(s).[0]
                )

    let private key (hash: string) (ruleset: string) = RedisKey(sprintf "leaderboard:%s:%s" hash ruleset)
    let private existence_key (hash: string) = RedisKey(sprintf "leaderboards:%s" hash)

    let add_score (hash: string) (ruleset: string) (userId: int64) (score: float) =
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
            (Replay.get_chart_scores hash ruleset userIds)

    let position (userId: int64) (hash: string) (ruleset: string) =
        let result = db.SortedSetRank(key hash ruleset, userId, Order.Descending)
        if result.HasValue then Some result.Value else None

    let score (userId: int64) (hash: string) (ruleset: string) =
        let result = db.SortedSetScore(key hash ruleset, userId)
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

        if migration < 2L then
            Logging.Debug("Performing migration 2")

            ft.Create(
                "idx:scores",
                FTCreateParams()
                    .On(IndexDataType.JSON)
                    .Prefix("score:"),
                Schema()
                    .AddTagField(new FieldName("$.RulesetId", "ruleset_id"), false)
                    .AddNumericField(new FieldName("$.UserId", "user_id"), false)
                    .AddNumericField(FieldName("$.Timestamp", "timestamp"), true)
                    .AddNumericField(FieldName("$.Score", "score"), true)
                    .AddNumericField(FieldName("$.Grade", "grade"), true)
                    .AddNumericField(FieldName("$.Lamp", "lamp"), true)
                    .AddNumericField(FieldName("$.Rate", "rate"), true)
            ) |> ignore

            db.StringIncrement("migration", 2L) |> ignore
            Logging.Debug("Migration 2 OK")
            
        if migration < 4L then
            Logging.Debug("Performing migration 3")
            
            ft.Alter(
                "idx:scores",
                Schema()
                    .AddTagField(FieldName("$.ChartId", "hash"), false)
            ) |> ignore
            
            db.StringIncrement("migration", 1L) |> ignore
            Logging.Debug("Migration 3 OK")