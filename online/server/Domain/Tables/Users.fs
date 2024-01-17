﻿namespace Interlude.Web.Server.Domain.New

open System
open Percyqaz.Common
open Prelude.Common
open Percyqaz.Data.Sqlite
open Interlude.Web.Server

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
        Color: int32
    }

module User =

    let TABLE : TableCommandHelper =
        {
            Name = "users"
            PrimaryKey = Column.Integer("Id").Unique
            Columns = 
                [
                    Column.Text("Username").Unique
                    Column.Text("DiscordId")
                    Column.Integer("DateSignedUp")
                    Column.Integer("LastLogin")
                    Column.Text("AuthToken")
                    Column.Text("Badges")
                    Column.Integer("Color")
                ]
        }
        
    let generate_auth_token() = Guid.NewGuid().ToString("N")
    let create (username, discord_id) =
        {
            Username = username
            DiscordId = discord_id
            DateSignedUp = DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()
            LastLogin = 0L
            AuthToken = generate_auth_token()
            Badges = Set.empty
            Color = Badge.DEFAULT_COLOR
        }

    let SAVE_NEW : NonQuery<User> =
        {
            SQL = TABLE.INSERT
            Parameters =
                [
                    "@Username", SqliteType.Text, -1
                    "@DiscordId", SqliteType.Text, -1
                    "@DateSignedUp", SqliteType.Integer, 8
                    "@LastLogin", SqliteType.Integer, 8
                    "@AuthToken", SqliteType.Text, -1
                    "@Badges", SqliteType.Text, -1
                    "@Color", SqliteType.Integer, 8
                ]
            FillParameters = (fun p user ->
                p.String user.Username
                p.String (string user.DiscordId)
                p.Int64 user.DateSignedUp
                p.Int64 user.LastLogin
                p.String user.AuthToken
                p.Json JSON user.Badges
                p.Int32 user.Color
            )
        }
    let save_new (user: User) : int64 = SAVE_NEW.ExecuteGetId user db |> expect

    let BY_DISCORD_ID : Query<uint64, int64 * User> =
        {
            SQL = """SELECT * FROM users WHERE DiscordId = @DiscordId;"""
            Parameters = [ "@DiscordId", SqliteType.Text, -1 ]
            FillParameters = (fun p id -> p.String (string id) )
            Read = (fun r -> 
                r.Int64,
                {
                    Username = r.String
                    DiscordId = uint64 r.String
                    DateSignedUp = r.Int64
                    LastLogin = r.Int64
                    AuthToken = r.String
                    Badges = r.Json JSON
                    Color = r.Int32
                }
            )
        }
    let by_discord_id (discord_id: uint64) = BY_DISCORD_ID.Execute discord_id db |> expect |> Array.tryExactlyOne

    let BY_ID : Query<int64, User> =
        {
            SQL = """SELECT * FROM users WHERE Id = @Id;"""
            Parameters = [ "@Id", SqliteType.Integer, 8 ]
            FillParameters = (fun p id -> p.Int64 id )
            Read = (fun r -> 
                r.Int64 |> ignore;
                {
                    Username = r.String
                    DiscordId = uint64 r.String
                    DateSignedUp = r.Int64
                    LastLogin = r.Int64
                    AuthToken = r.String
                    Badges = r.Json JSON
                    Color = r.Int32
                }
            )
        }
    let by_id (id: int64) = BY_ID.Execute id db |> expect |> Array.tryExactlyOne

    let by_ids (ids: int64 array) =
        if ids.Length = 0 then
            [||]
        else

        let ids_string = String.concat "," (Array.map string ids)
        let query : Query<unit, int64 * User> =
            { Query.without_parameters() with
                SQL = sprintf "SELECT * FROM users WHERE Id IN (%s)" ids_string
                Read = (fun r -> 
                    r.Int64,
                    {
                        Username = r.String
                        DiscordId = uint64 r.String
                        DateSignedUp = r.Int64
                        LastLogin = r.Int64
                        AuthToken = r.String
                        Badges = r.Json JSON
                        Color = r.Int32
                    }
                )
            }
        query.Execute () db |> expect

    let BY_AUTH_TOKEN : Query<string, int64 * User> =
        {
            SQL = """SELECT * FROM users WHERE AuthToken = @AuthToken;"""
            Parameters = [ "@AuthToken", SqliteType.Text, -1 ]
            FillParameters = (fun p token -> p.String token )
            Read = (fun r -> 
                r.Int64,
                {
                    Username = r.String
                    DiscordId = uint64 r.String
                    DateSignedUp = r.Int64
                    LastLogin = r.Int64
                    AuthToken = r.String
                    Badges = r.Json JSON
                    Color = r.Int32
                }
            )
        }
    let by_auth_token (token: string) = BY_AUTH_TOKEN.Execute token db |> expect |> Array.tryExactlyOne

    let BY_USERNAME : Query<string, int64 * User> =
        {
            SQL = """SELECT * FROM users WHERE Username LIKE @Username ESCAPE '\';"""
            Parameters = [ "@Username", SqliteType.Text, -1 ]
            FillParameters = (fun p username -> p.String (username.Replace("_", "\\_")) )
            Read = (fun r -> 
                r.Int64,
                {
                    Username = r.String
                    DiscordId = uint64 r.String
                    DateSignedUp = r.Int64
                    LastLogin = r.Int64
                    AuthToken = r.String
                    Badges = r.Json JSON
                    Color = r.Int32
                }
            )
        }
    let by_username (username: string) = BY_USERNAME.Execute username db |> expect |> Array.tryExactlyOne

    let SEARCH_BY_USERNAME : Query<string, int64 * User> =
        {
            SQL = """SELECT * FROM users WHERE Username LIKE @Pattern ESCAPE '\' ORDER BY LastLogin DESC;"""
            Parameters = [ "@Pattern", SqliteType.Text, -1 ]
            FillParameters = (fun p query -> p.String ("%" + query.Replace("_", "\\_") + "%") )
            Read = (fun r -> 
                r.Int64,
                {
                    Username = r.String
                    DiscordId = uint64 r.String
                    DateSignedUp = r.Int64
                    LastLogin = r.Int64
                    AuthToken = r.String
                    Badges = r.Json JSON
                    Color = r.Int32
                }
            )
        }
    let search_by_username (query: string) = SEARCH_BY_USERNAME.Execute query db |> expect
    
    let LIST : Query<int, int64 * User> =
        {
            SQL = """SELECT * FROM users ORDER BY DateSignedUp ASC LIMIT @Limit OFFSET @Offset;"""
            Parameters = [ "@Limit", SqliteType.Integer, 8; "@Offset", SqliteType.Integer, 8 ]
            FillParameters = (fun p page -> p.Int64 15L; p.Int64 (int64 page * 15L) )
            Read = (fun r -> 
                r.Int64,
                {
                    Username = r.String
                    DiscordId = uint64 r.String
                    DateSignedUp = r.Int64
                    LastLogin = r.Int64
                    AuthToken = r.String
                    Badges = r.Json JSON
                    Color = r.Int32
                }
            )
        }
    let list (page: int) = if page < 0 then [||] else LIST.Execute page db |> expect

    let SET_AUTH_TOKEN : NonQuery<int64 * string> =
        {
            SQL = """UPDATE users SET AuthToken = @AuthToken WHERE Id = @Id;"""
            Parameters = [ "@AuthToken", SqliteType.Text, -1; "@Id", SqliteType.Integer, 8 ]
            FillParameters = fun p (id, token) -> p.String token; p.Int64 id
        }
    let set_auth_token (id, token: string) = SET_AUTH_TOKEN.Execute (id, token) db |> expect |> ignore
    
    let UPDATE_COLOR : NonQuery<int64 * int32> =
        {
            SQL = """UPDATE users SET Color = @Color WHERE Id = @Id;"""
            Parameters = [ "@Color", SqliteType.Integer, 4; "@Id", SqliteType.Integer, 8 ]
            FillParameters = fun p (id, color) -> p.Int32 color; p.Int64 id
        }
    let update_color (id, color: int32) = UPDATE_COLOR.Execute (id, color) db |> expect |> ignore
    
    let UPDATE_BADGES : NonQuery<int64 * Set<Badge>> =
        {
            SQL = """UPDATE users SET Badges = @Badges WHERE Id = @Id;"""
            Parameters = [ "@Badges", SqliteType.Text, -1; "@Id", SqliteType.Integer, 8 ]
            FillParameters = fun p (id, badges) -> p.Json JSON badges; p.Int64 id
        }
    let update_badges (id, badges: Set<Badge>) = UPDATE_BADGES.Execute (id, badges) db |> expect |> ignore

    let UPDATE_LAST_SEEN : NonQuery<int64> =
        {
            SQL = """UPDATE users SET LastLogin = @Now WHERE Id = @Id;"""
            Parameters = [ "@Now", SqliteType.Integer, 8; "@Id", SqliteType.Integer, 8 ]
            FillParameters = fun p id -> p.Int64 (DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()); p.Int64 id
        }
    let update_last_seen (id) = UPDATE_LAST_SEEN.Execute (id) db |> expect |> ignore