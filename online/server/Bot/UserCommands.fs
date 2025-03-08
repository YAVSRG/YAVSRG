namespace Interlude.Web.Server.Bot

open System
open Discord
open Discord.WebSocket
open Prelude.Gameplay.Rulesets
open Prelude.Backbeat.Archive
open Interlude.Web.Server.Domain.Core
open Interlude.Web.Server.Domain.Services

module UserCommands =

    let user_by_name (name: string) =
        if Seq.forall (fun (c: char) -> Seq.contains c Users.Username.VALID_CHARACTERS) name then
            User.by_username name
        else
            None

    // Requires user to exist but no particular permissions
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
            | "fl"
            | "friends"
            | "friendlist" ->
                let friends = Friends.friends_list (user_id)

                let embed =
                    EmbedBuilder(Title = sprintf "%s's friends list" user_info.Username)
                        .WithColor(Color.Blue)
                        .WithDescription(
                            if friends.Length = 0 then
                                "nobody :("
                            else
                                friends |> Array.map (fun (id, user) -> user.Username) |> String.concat "\n"
                        )

                do! reply_embed (embed.Build())
            | "f"
            | "friend" ->
                match args with
                | [] -> do! reply "Enter a username, for example: $friend Percyqaz"
                | username :: _ ->
                    match user_by_name username with
                    | Some(id, _) ->
                        match Friends.add (user_id, id) with
                        | Ok() -> do! reply_emoji ":white_check_mark:"
                        | Error reason -> do! reply reason
                    | None -> do! reply "No user found."
            | "uf"
            | "unfriend" ->
                match args with
                | [] -> do! reply "Enter a username, for example: $unfriend Percyqaz"
                | username :: _ ->
                    match user_by_name username with
                    | Some(id, _) ->
                        Friends.remove (user_id, id)
                        do! reply_emoji ":white_check_mark:"
                    | None -> do! reply "No user found."
            | "pc"
            | "profilecolor" ->
                match args with
                | [] -> do! reply "Enter a badge name, for example $profilecolor early-tester"
                | badge :: [] ->
                    if user_info.Badges.Contains badge then
                        let color = (Badge.badge_color badge).[0]
                        User.update_color (user_id, color)
                        do! reply_emoji ":white_check_mark:"
                    else
                        do! reply "You don't have this badge."
                | badge :: choice :: _ ->
                    if user_info.Badges.Contains badge then
                        let colors = Badge.badge_color badge

                        match System.Int32.TryParse(choice) with
                        | true, c when c > 0 && c <= colors.Length ->
                            User.update_color (user_id, colors.[c - 1])

                            do! reply_emoji ":white_check_mark:"
                        | _ -> do! reply (sprintf "The options for this badge are 1-%i" colors.Length)
                    else
                        do! reply "You don't have this badge."
            | "p"
            | "profile" ->
                let profile (user_id, user_info) =
                    task {
                        let now = DateTimeOffset.UtcNow

                        let format_time_ago (timestamp: int64) =
                            let ts: TimeSpan = now - DateTimeOffset.FromUnixTimeMilliseconds(timestamp)

                            if ts.TotalDays > 365.0 then
                                sprintf "%.0fy ago" (ts.TotalDays / 365.0)
                            elif ts.TotalDays > 30.0 then
                                sprintf "%.0fmo ago" (ts.TotalDays / 30.0)
                            elif ts.TotalDays > 7.0 then
                                sprintf "%.0fw ago" (ts.TotalDays / 7.0)
                            elif ts.TotalDays > 1.0 then
                                sprintf "%.0fd ago" ts.TotalDays
                            elif ts.TotalHours > 1.0 then
                                sprintf "%.0fh ago" ts.TotalHours
                            elif ts.TotalMinutes > 5.0 then
                                sprintf "%.0fm ago" ts.TotalMinutes
                            else
                                "Just now"

                        let format_mods (score: Score.RecentScore) =
                            if score.Mods.IsEmpty then
                                sprintf "%.2fx" score.Rate
                            else
                                sprintf "%.2fx*" score.Rate

                        let recent_scores = Score.get_user_recent user_id

                        let embed =
                            let color = user_info.Color |> Drawing.Color.FromArgb |> Color.op_Explicit

                            let embed =
                                EmbedBuilder(
                                    Title = user_info.Username,
                                    Footer =
                                        EmbedFooterBuilder(
                                            Text = (String.concat ", " user_info.Badges).Replace("-", " ").ToUpper()
                                        )
                                )
                                    .WithColor(color)

                            if recent_scores.Length > 0 then
                                embed.WithFields(
                                    EmbedFieldBuilder(Name = "Recent scores", IsInline = true)
                                        .WithValue(
                                            recent_scores
                                            |> Array.map (fun s ->
                                                match Backbeat.Charts.by_hash s.ChartId with
                                                | Some(_, song) -> song.Title
                                                | None -> "???" |> sprintf "`%-20s`"
                                            )
                                            |> String.concat "\n"
                                        ),
                                    EmbedFieldBuilder(Name = "..", IsInline = true)
                                        .WithValue(
                                            recent_scores
                                            |> Array.map (fun (s: Score.RecentScore) ->
                                                sprintf
                                                    "`%6.2f%%` `%6s` `%6s` `%8s`"
                                                    (s.Accuracy * 100.0)
                                                    (SC_J4.LampName s.Lamp)
                                                    (format_mods s)
                                                    (format_time_ago s.TimePlayed)
                                            )
                                            |> String.concat "\n"
                                        )
                                )
                            else
                                embed

                        do! reply_embed (embed.Build())
                    }

                match args with
                | [] -> do! profile (user_id, user_info)
                | name :: _ ->
                    match user_by_name name with
                    | Some(id, user) -> do! profile (id, user)
                    | None -> do! reply "No user found."

            | "help" -> do! reply "Available commands: $search, $friends, $friend, $unfriend, $profilecolor, $profile"

            | _ -> ()
        }