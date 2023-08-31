namespace Interlude.Web.Server.Bot

open System
open Discord
open Discord.WebSocket
open Prelude.Backbeat.Archive
open Interlude.Web.Server
open Interlude.Web.Server.Domain
open Interlude.Web.Server.Online

module Commands =

    let user_by_name (name: string) =
        if Seq.forall (fun (c: char) -> Seq.contains c Users.VALID_USERNAME_CHARACTERS) name then User.by_username name else None

    // Requires user to exist but no particular permissions
    let user_dispatch (client: DiscordSocketClient) (userId: int64, userInfo: User) (context: SocketMessage) (command: string) (args: string list) =
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
            | "s"
            | "search" ->
                match args with

                | [] -> 
                    do! reply "Enter a search term, for example: $search PLANET//SHAPER artist:Camellia creator:Evening"

                | query :: _ -> 
                    let matches = Charts.search query |> List.ofSeq
                    match matches with
                    | [] -> 
                        do! reply "No matches found."
                    | (song, charts) :: [] ->
                        let embed = 
                            EmbedBuilder(Title = song.Title)
                                .AddField((if song.Artists.Length > 1 then "Artists" else "Artist"), String.concat ", " song.Artists, true)
                        if song.OtherArtists <> [] then
                            embed.AddField("Featuring", String.concat ", " song.OtherArtists, true) |> ignore
                        if song.Remixers <> [] then
                            embed.AddField("Remixed by", String.concat ", " song.Remixers, true) |> ignore
                        if song.Tags <> [] then
                            embed.AddField("Tags", String.concat ", " song.Tags) |> ignore
                        if song.Source <> None then
                            embed.AddField("Source", song.Source.Value) |> ignore
                        embed.WithColor(Color.Blue) |> ignore
                        for chart in charts do
                            embed.AddField(chart.DifficultyName + " by " + String.concat ", " chart.Creators, String.concat "  |  " (chart.Sources |> List.map Charts.format_source)) |> ignore
                        do! reply_embed (embed.Build())
                    | _ ->
                        let embed =
                            EmbedBuilder(Title = match matches.Length with 30 -> "30+ matches found" | i -> sprintf "%i matches found" i)
                                .WithDescription(String.concat "\n" (List.map (fun (song: Song, charts) -> song.FormattedTitle.Replace("*", "\\*")) matches))
                                .WithColor(Color.Blue)
                        do! reply_embed (embed.Build())
            | "fl"
            | "friends"
            | "friendlist" ->
                let friends = Friends.friends_list(userId)
                let embed = 
                    EmbedBuilder(Title = sprintf "%s's friends list" userInfo.Username)
                        .WithColor(Color.Blue)
                        .WithDescription(
                        if friends.Length = 0 then "nobody :(" else
                        friends
                        |> Array.map (function Some user -> user.Username | None -> "<deleted user>")
                        |> String.concat "\n"
                    )
                do! reply_embed (embed.Build())
            | "f"
            | "friend" ->
                match args with
                | [] -> do! reply "Enter a username, for example: $friend Percyqaz"
                | username :: _ ->
                    match user_by_name username with
                    | Some (id, _) ->
                        Friends.add_friend(userId, id)
                        do! reply_emoji ":white_check_mark:"
                    | None -> do! reply "No user found."
            | "uf"
            | "unfriend" ->
                match args with
                | [] -> do! reply "Enter a username, for example: $unfriend Percyqaz"
                | username :: _ ->
                    match user_by_name username with
                    | Some (id, _) ->
                        Friends.remove_friend(userId, id)
                        do! reply_emoji ":white_check_mark:"
                    | None -> do! reply "No user found."
            | "pc"
            | "profilecolor" ->
                match args with
                | [] -> do! reply "Enter a badge name, for example $profilecolor early-tester"
                | badge :: [] ->
                    if userInfo.Badges.Contains badge then
                        let color = (Badge.badge_color badge).[0]
                        User.save(userId, { userInfo with Color = Some color })
                        do! reply_emoji ":white_check_mark:"
                    else do! reply "You don't have this badge."
                | badge :: choice :: _ ->
                    if userInfo.Badges.Contains badge then
                        let colors = Badge.badge_color badge
                        match System.Int32.TryParse(choice) with
                        | true, c when c > 0 && c <= colors.Length ->
                            User.save(userId, { userInfo with Color = Some colors.[c - 1] })
                            do! reply_emoji ":white_check_mark:"
                        | _ -> do! reply (sprintf "The options for this badge are 1-%i" colors.Length)
                    else do! reply "You don't have this badge."
            | "p"
            | "profile" ->
                
                let now = DateTimeOffset.UtcNow
                let formatTimeOffset (ts: int64) =
                   let ts: TimeSpan = now - DateTimeOffset.FromUnixTimeMilliseconds(ts)
                   if ts.TotalDays > 365.0 then sprintf "%.0fy ago" (ts.TotalDays / 365.0)
                   elif ts.TotalDays > 30.0 then sprintf "%.0fmo ago" (ts.TotalDays / 30.0)
                   elif ts.TotalDays > 7.0 then sprintf "%.0fw ago" (ts.TotalDays / 7.0)
                   elif ts.TotalDays > 1.0 then sprintf "%.0fd ago" ts.TotalDays
                   elif ts.TotalHours > 1.0 then sprintf "%.0fh ago" ts.TotalHours
                   elif ts.TotalMinutes > 5.0 then sprintf "%.0fm ago" ts.TotalMinutes
                   else "Just now"
                let formatMods (score: Score) =
                    if score.Mods.IsEmpty then sprintf "%.2fx" score.Rate else sprintf "%.2fx*" score.Rate

                let recent_scores = Score.get_recent userId
                let embed = 
                    let color = 
                        userInfo.Color
                        |> Option.defaultValue Badge.DEFAULT_COLOR
                        |> Drawing.Color.FromArgb
                        |> Color.op_Explicit
                    EmbedBuilder(Title = userInfo.Username, Footer = EmbedFooterBuilder(Text = (String.concat ", " userInfo.Badges).Replace("-", " ").ToUpper()))
                        .WithColor(color)
                        .WithFields(
                            EmbedFieldBuilder(Name = "Recent scores", IsInline = true)
                                .WithValue(
                                    recent_scores
                                    |> Array.map(fun s -> match Charts.by_hash s.ChartId with Some (_, song) -> song.Title | None -> "???" |> sprintf "`%-30s`")
                                    |> String.concat "\n"
                                ),
                            EmbedFieldBuilder(Name = "..", IsInline = true)
                                .WithValue(
                                    recent_scores
                                    |> Array.map(fun s -> sprintf "`%10.2f%%` `%10s` `%10s` `%10s`" (s.Score * 100.0) (Charts.rulesets.[s.RulesetId].LampName s.Lamp) (formatTimeOffset s.Timestamp) (formatMods s))
                                    |> String.concat "\n"
                                )
                            )
                do! reply_embed (embed.Build())

            | "help" ->
                do! reply "Available commands: $search, $friends, $friend, $unfriend, $profilecolor, $profile"
                    
            | _ -> ()
        }

    // Requires user to exist, with the developer role
    let admin_dispatch (client: DiscordSocketClient) (userId: int64, userInfo: User) (context: SocketMessage) (command: string) (args: string list) =
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
                | [] ->  do! reply "Enter a username, for example: $user Percyqaz"
                | name :: _ -> 
                    match user_by_name name with
                    | Some (id, user) -> 
                        let embed = 
                            EmbedBuilder(Title = user.Username)
                                .AddField("User ID", id.ToString(), false)
                                .AddField("Signed up", DateTimeOffset.FromUnixTimeMilliseconds(user.DateSignedUp).ToString("yyyy/MM/dd H:mm:ss"), false)
                                .AddField("Last login", DateTimeOffset.FromUnixTimeMilliseconds(user.LastLogin).ToString("yyyy/MM/dd H:mm:ss"), false)
                                .AddField("Badges", (if user.Badges.IsEmpty then "[ no badges ]" else String.concat ", " user.Badges), false)
                                .WithColor(Color.Blue)
                        do! reply_embed (embed.Build())
                    | None -> do! reply "No user found."

            | "users" ->
                let user_list = User.all()
                do!
                    EmbedBuilder(Title = "All registered users")
                        .WithDescription(user_list |> Seq.map snd |> Seq.map (fun user -> user.Username) |> String.concat "\n")
                        .WithColor(Color.Blue)
                        .Build()
                    |> reply_embed

            | "online" ->
                let! user_list = LoggedInUsers.who_is_online()
                do!
                    EmbedBuilder(Title = sprintf "All online users (%i)" user_list.Length)
                        .WithDescription(if user_list.Length = 0 then "nobody :(" else user_list |> Seq.map snd |> String.concat "\n")
                        .WithColor(Color.Blue)
                        .Build()
                    |> reply_embed

            | "addbadge" ->
                match args with
                | []
                | _ :: [] -> do! reply "Enter a user and badge, for example: $addbadge Percyqaz$developer"
                | name :: badge :: _ -> 
                    match user_by_name name with
                    | Some (id, user) ->
                        User.save(id, { user with Badges = Set.add badge user.Badges })
                        do! reply_emoji ":white_check_mark:"
                    | None -> do! reply "No user found."

            | "removebadge" ->
                match args with
                | []
                | _ :: [] -> do! reply "Enter a user and badge, for example: $removebadge Percyqaz$developer"
                | name :: badge :: _ -> 
                    match user_by_name name with
                    | Some (id, user) ->
                        User.save(id, { user with Badges = Set.remove badge user.Badges })
                        do! reply_emoji ":white_check_mark:"
                    | None -> do! reply "No user found."

            | "contact" ->
                match args with
                | [] -> do! reply "Enter a username, for example: $contact Percyqaz"
                | name :: _ ->
                    match user_by_name name with
                    | Some (id, user) ->
                        do! reply (sprintf "<@%i>" user.DiscordId)
                    | None -> do! reply "No user found."

            | "removeuser" ->
                match args with
                | [] -> do! reply "Enter a username, for example: $removeuser Percyqaz"
                | name :: _ ->
                    match user_by_name name with
                    | Some (id, user) ->
                        Aggregate.delete_user(id)
                        do! reply_emoji ":white_check_mark:"
                    | None -> do! reply "No user found."

            | "addtestuser" when not SECRETS.IsProduction ->
                match args with
                | [] -> do! reply "Enter a username, for example: $addtestuser TESTUSER1"
                | name :: _ ->

                    match Users.valid_username name with
                    | Error reason -> do! reply (sprintf "Invalid username (%s)" reason)
                    | Ok() -> 

                    match User.by_username(name) with
                    | Some u -> do! reply "That username is already taken"
                    | None ->

                    let user = User.create(name, 0uL)
                    let id = User.save_new(user)
                    do! reply (sprintf "Created user '%s' with id '%i'\n\nAuth token:\n%s" user.Username id user.AuthToken)

            | "reloadbackbeat" ->
                Charts.init()
                do! reply_emoji ":white_check_mark:"

            | "addleaderboard" ->
                match args with
                | [] -> do! reply "Enter a hash and ruleset, for example: $addleaderboard 1467CD6DEB4A3B87FA58FAB4F2398BE9AD7B0017031C511C549D3EF28FFB58D3"
                | hash :: _ ->
                    match Charts.by_hash hash with
                    | None -> do! reply "Chart not found."
                    | Some (chart, song) ->
                    Leaderboard.create hash "SC(J4)548E5A"
                    let msg = sprintf "## :checkered_flag: Leaderboard created:\n%s [%s]" song.FormattedTitle chart.DifficultyName
                    let! _ = (client.GetChannel(FEED_CHANNEL_ID) :?> SocketTextChannel).SendMessageAsync(msg)
                    do! reply_emoji ":white_check_mark:"

            | _ -> ()
        }