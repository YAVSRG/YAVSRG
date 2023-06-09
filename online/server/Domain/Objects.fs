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

type User =
    {
        Username: string
        DiscordId: uint64
        DateSignedUp: int64
        LastLogin: int64
        AuthToken: string
        Badges: Set<Badge>
    }

module User =

    let id (key: string) = key.Substring(5) |> int64
    let key (id: int64) = RedisKey("user:" + id.ToString())
    let escape (username: string) = username.Replace("'", "\\'").Replace("-", "\\-")

    let create(username, discord_id) =
        { 
            Username = username
            DiscordId = discord_id
            DateSignedUp = DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()
            LastLogin = 0L
            AuthToken = Guid.NewGuid().ToString("N")
            Badges = Set.empty
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
        json.MGet([| key id |], "$")
        |> Array.tryExactlyOne
        |> Option.map (fun r -> Text.Json.JsonSerializer.Deserialize<User>(r.ToString()))

    let by_auth_token(token: string) =
        let results = ft.Search("idx:users", Query(sprintf "@auth_token:{%s}" token)).Documents
        Seq.tryExactlyOne results
        |> Option.map (fun d -> id d.Id, Text.Json.JsonSerializer.Deserialize<User>(d.Item "json"))
        
    let by_username(username: string) =
        let results = ft.Search("idx:users", Query(sprintf "@username:{%s}" (escape username))).Documents
        Seq.tryExactlyOne results
        |> Option.map (fun d -> id d.Id, Text.Json.JsonSerializer.Deserialize<User>(d.Item "json"))

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

        //if migration < 2L then
        //    Logging.Debug("Performing migration 2")

        //    // next migration
        //    db.StringIncrement("migration", 1L) |> ignore
        //    Logging.Debug("Migration 2 OK")
        
        //if migration < 3L then
        //    Logging.Debug("Performing migration 3")
        //    // next migration
        //    db.StringIncrement("migration", 1L) |> ignore
        //    Logging.Debug("Migration 3 OK")