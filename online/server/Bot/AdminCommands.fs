namespace Interlude.Web.Server.Bot

open System
open Discord
open Discord.WebSocket
open Percyqaz.Common
open Prelude.Data.User.Stats
open Interlude.Web.Server.Domain.Backbeat
open Interlude.Web.Server.Domain.Core
open Interlude.Web.Server.Domain.Services
open Interlude.Web.Server.Online

module AdminCommands =

    let user_by_name (name: string) =
        if Seq.forall (fun (c: char) -> Seq.contains c Users.Username.VALID_CHARACTERS) name then
            User.by_username name
        else
            None

    // Requires user to exist, with the developer role
    let dispatch
        (client: DiscordSocketClient)
        (user_id: int64, user_info: User)
        (context: SocketMessage)
        (command: string)
        (args: string list)
        =
        task {
            let reply msg =
                task {
                    let! _ = context.Channel.SendMessageAsync(msg)
                    return ()
                }

            let reply_embed embed =
                task {
                    let! _ = context.Channel.SendMessageAsync(embed = embed)
                    return ()
                }

            let reply_emoji emoji =
                task {
                    let! _ = context.AddReactionAsync(Emoji.Parse emoji)
                    return ()
                }

            match command with
            | "user" ->
                match args with
                | [] -> do! reply "Enter a username, for example: $user Percyqaz"
                | name :: _ ->
                    match user_by_name name with
                    | Some(id, user) ->
                        let embed =
                            EmbedBuilder(Title = user.Username)
                                .AddField("User ID", id.ToString(), false)
                                .AddField(
                                    "Signed up",
                                    DateTimeOffset
                                        .FromUnixTimeMilliseconds(user.DateSignedUp)
                                        .ToString("yyyy/MM/dd H:mm:ss"),
                                    false
                                )
                                .AddField(
                                    "Last login",
                                    DateTimeOffset
                                        .FromUnixTimeMilliseconds(user.LastLogin)
                                        .ToString("yyyy/MM/dd H:mm:ss"),
                                    false
                                )
                                .AddField(
                                    "Badges",
                                    (if user.Badges.IsEmpty then
                                         "[ no badges ]"
                                     else
                                         String.concat ", " user.Badges),
                                    false
                                )
                                .WithColor(Color.Blue)

                        do! reply_embed (embed.Build())
                    | None -> do! reply "No user found."

            | "users" ->
                let embed, components = AdminInteractables.user_list 0
                let! _ = context.Channel.SendMessageAsync(embed = embed, components = components)
                ()

            | "online" ->
                let user_list = Session.list_online_users ()
                let lobby_count = Lobby.count()

                do!
                    EmbedBuilder(Title = sprintf "All online users (%i)" user_list.Length)
                        .WithDescription(
                            if user_list.Length = 0 then
                                "nobody :("
                            else
                                user_list |> Seq.map snd |> String.concat "\n"
                        )
                        .WithFooter(sprintf "%i multiplayer lobbies in progress" lobby_count)
                        .WithColor(Color.Blue)
                        .Build()
                    |> reply_embed

            | "addbadge" ->
                match args with
                | []
                | _ :: [] -> do! reply "Enter a user and badge, for example: $addbadge Percyqaz$developer"
                | name :: badge :: _ ->
                    match user_by_name name with
                    | Some(id, user) ->
                        User.update_badges (id, Set.add badge user.Badges)

                        do! reply_emoji ":white_check_mark:"
                    | None -> do! reply "No user found."

            | "removebadge" ->
                match args with
                | []
                | _ :: [] -> do! reply "Enter a user and badge, for example: $removebadge Percyqaz$developer"
                | name :: badge :: _ ->
                    match user_by_name name with
                    | Some(id, user) ->
                        User.update_badges (id, Set.remove badge user.Badges)

                        do! reply_emoji ":white_check_mark:"
                    | None -> do! reply "No user found."

            | "whois" ->
                match args with
                | [] -> do! reply "Enter a username, for example: $whois Percyqaz"
                | name :: _ ->
                    match user_by_name name with
                    | Some(id, user) -> do! reply (sprintf "<@%i>" user.DiscordId)
                    | None -> do! reply "No user found."

            | "removeuser" ->
                match args with
                | [] -> do! reply "Enter a username, for example: $removeuser Percyqaz"
                | name :: _ ->
                    match user_by_name name with
                    | Some(id, user) ->
                        Users.permanently_delete_user (id)
                        do! reply_emoji ":white_check_mark:"
                    | None -> do! reply "No user found."

            | "renameuser" ->
                match args with
                | []
                | [_] -> do! reply "Enter an existing username and new username, for example: $renameuser Percyqaz$Bananahira"
                | old_name :: new_name :: _ ->
                    match Users.Auth.rename old_name new_name with
                    | Ok() -> do! reply_emoji ":white_check_mark:"
                    | Error reason -> do! reply reason

            | "search" ->
                match args with

                | [] ->
                    do! reply "Enter a search term, for example: $search PLANET//SHAPER"

                | query :: [] ->
                    let embed, components = UserInteractables.song_search query 0 false
                    let! _ = context.Channel.SendMessageAsync(embed = embed, components = components)
                    return ()

                | query :: page :: [] ->
                    let embed, components = UserInteractables.song_search query (int page - 1) false
                    let! _ = context.Channel.SendMessageAsync(embed = embed, components = components)
                    return ()

                | query :: page :: _ ->
                    let embed, components = UserInteractables.song_search query (int page - 1) true
                    let! _ = context.Channel.SendMessageAsync(embed = embed, components = components)
                    return ()

            | "merge_songs" ->
                match args with
                | duplicate :: original :: [] ->
                    if Songs.merge_songs (int64 original) (int64 duplicate) then
                        do! reply_emoji ":white_check_mark:"
                    else do! reply_emoji ":x:"
                | _ -> do! reply "Enter two song ids, for example: $merge <id of duplicate>$<id of original>"

            | "stats" ->
                let total_gametime = Stats.sum_gametime ()
                let total_playtime = Stats.sum_playtime ()
                let month = Timestamp.now() |> timestamp_to_leaderboard_month
                let monthly_playtime = MonthlyStats.sum_playtime month

                do!
                    EmbedBuilder(Title = "Interlude server stats")
                        .WithDescription(
                            sprintf "Total time game open: %s\nTotal time playing: %s\nMonthly playtime: %s"
                                (format_long_time total_gametime)
                                (format_long_time total_playtime)
                                (format_long_time monthly_playtime)
                        )
                        .WithColor(Color.Blue)
                        .Build()
                    |> reply_embed
            | _ -> ()
        }