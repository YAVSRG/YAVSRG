namespace Interlude.Web.Server.Domain

open System
open StackExchange.Redis
open NRedisStack.Search
open Interlude.Web.Server.Domain.Redis

type Badge = string

module Badge =

    let DEVELOPER = "developer"
    let DONATOR = "donator"
    let MODERATOR = "moderator"
    let EARLYTESTER = "early-tester"
    let TABLE_EDITOR = "table-editor"

    let badge_color (badge: Badge) : int32 list =
        match badge with
        | _ when badge = EARLYTESTER -> [ 0xFF_66ff6e ]
        | _ when badge = MODERATOR -> [ 0xFF_66c2ff ]
        | _ when badge = DEVELOPER -> [ 0xFF_ff7559 ]
        | _ when badge = DONATOR -> [ 0xFF_ff8cdd; 0xFF_ffd36e ]
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

    let create (username, discord_id) =
        {
            Username = username
            DiscordId = discord_id
            DateSignedUp = DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()
            LastLogin = 0L
            AuthToken = Guid.NewGuid().ToString("N")
            Badges = Set.empty
            Color = None
        }

    let set_auth_token (id, token: string) =
        json.Set(key id, "$.AuthToken", sprintf "\"%s\"" token) |> ignore

    let update_color (id, color: int32) =
        json.Set(key id, "$.Color", Some color) |> ignore

    let update_badges (id, badges: Set<Badge>) =
        json.Set(key id, "$.Badges", badges) |> ignore

    let update_last_seen (id) =
        json.Set(key id, "$.LastLogin", Some(DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()))
        |> ignore

    let save_new (user: User) : int64 =
        let new_id = db.StringIncrement("count:users", 1L)
        json.Set(key new_id, "$", user) |> ignore
        new_id

    let by_discord_id (discord_id: uint64) =
        let results =
            ft
                .Search("idx:users", Query(sprintf "@discord_id:[%i %i]" discord_id discord_id))
                .Documents

        Seq.tryExactlyOne results
        |> Option.map (fun d -> id d.Id, Text.Json.JsonSerializer.Deserialize<User>(d.Item "json"))

    let by_id (id: int64) =
        let result = json.Get(key id, [| "$" |])

        if result.IsNull then
            None
        else
            let s: string = RedisResult.op_Explicit (result)
            Some <| Text.Json.JsonSerializer.Deserialize<User array>(s).[0]

    let by_ids (ids: int64 array) =
        if ids.Length = 0 then
            [||]
        else

            json.MGet(ids |> Array.map key, "$")
            |> Array.map (fun result ->
                if result.IsNull then
                    None
                else
                    let s: string = RedisResult.op_Explicit result
                    Some <| Text.Json.JsonSerializer.Deserialize<User array>(s).[0]
            )

    let by_auth_token (token: string) =
        let token = escape token

        if token = "" then
            None
        else

            let results =
                ft.Search("idx:users", Query(sprintf "@auth_token:{%s}" token)).Documents

            Seq.tryExactlyOne results
            |> Option.map (fun d -> id d.Id, Text.Json.JsonSerializer.Deserialize<User>(d.Item "json"))

    let by_username (username: string) =
        let username = escape username

        if username = "" then
            None
        else

            let results =
                ft.Search("idx:users", Query(sprintf "@username:{%s}" username)).Documents

            Seq.tryExactlyOne results
            |> Option.map (fun d -> id d.Id, Text.Json.JsonSerializer.Deserialize<User>(d.Item "json"))

    let search_by_username (query: string) =
        let query = escape query

        if query = "" then
            [||]
        else

            ft
                .Search("idx:users", Query(sprintf "@username:{*%s*}" query).SetSortBy("last_login", false))
                .Documents
            |> Seq.map (fun d -> id d.Id, Text.Json.JsonSerializer.Deserialize<User>(d.Item "json"))
            |> Array.ofSeq

    let list (page: int) =
        ft
            .Search("idx:users", Query("*").SetSortBy("date_signed_up", true).Limit(page * 15, 15))
            .Documents
        |> Seq.map (fun d -> id d.Id, Text.Json.JsonSerializer.Deserialize<User>(d.Item "json"))
        |> Array.ofSeq

    let _dump () =
        ft
            .Search("idx:users", Query("*").SetSortBy("date_signed_up", true))
            .Documents
        |> Seq.map (fun d -> id d.Id, Text.Json.JsonSerializer.Deserialize<User>(d.Item "json"))
        |> Array.ofSeq
