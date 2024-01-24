namespace Interlude.Web.Server.Bot

open System
open Discord
open Discord.WebSocket
open Prelude.Backbeat.Archive
open Interlude.Web.Server
open Interlude.Web.Server.Domain
open Interlude.Web.Server.Online

module AdminCommands =

    let user_by_name (name: string) =
        if Seq.forall (fun (c: char) -> Seq.contains c Users.VALID_USERNAME_CHARACTERS) name then
            User.by_username name
        else
            None

    let user_list (page: int) =
        let user_list = User.list (page)

        let embed =
            EmbedBuilder(Title = "All registered users")
                .WithDescription(
                    if user_list.Length = 0 then
                        "Empty page :("
                    else
                        user_list
                        |> Seq.map snd
                        |> Seq.map (fun user -> user.Username)
                        |> String.concat "\n"
                )
                .WithColor(Color.Blue)
                .Build()

        let components =
            ComponentBuilder()
                .AddRow(
                    ActionRowBuilder()
                        .WithButton(
                            ButtonBuilder(Emote = Emoji.Parse(":arrow_left:"), CustomId = sprintf "users %i" (page - 1))
                                .WithStyle(ButtonStyle.Secondary)
                                .WithDisabled(page <= 0)
                        )
                        .WithButton(
                            ButtonBuilder(
                                Emote = Emoji.Parse(":arrow_right:"),
                                CustomId = sprintf "users %i" (page + 1)
                            )
                                .WithStyle(ButtonStyle.Secondary)
                                .WithDisabled(user_list.Length < 15)
                        )
                )
                .Build()

        embed, components

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
                let embed, components = user_list 0
                let! _ = context.Channel.SendMessageAsync(embed = embed, components = components)
                ()

            | "online" ->
                let user_list = Session.list_online_users ()

                do!
                    EmbedBuilder(Title = sprintf "All online users (%i)" user_list.Length)
                        .WithDescription(
                            if user_list.Length = 0 then
                                "nobody :("
                            else
                                user_list |> Seq.map snd |> String.concat "\n"
                        )
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

            | "contact" ->
                match args with
                | [] -> do! reply "Enter a username, for example: $contact Percyqaz"
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
                        Aggregate.delete_user (id)
                        do! reply_emoji ":white_check_mark:"
                    | None -> do! reply "No user found."

            | "addtestuser" when not SECRETS.IsProduction ->
                match args with
                | [] -> do! reply "Enter a username, for example: $addtestuser TESTUSER1"
                | name :: _ ->

                    match Users.valid_username name with
                    | Error reason -> do! reply (sprintf "Invalid username (%s)" reason)
                    | Ok() ->

                    match User.by_username (name) with
                    | Some u -> do! reply "That username is already taken"
                    | None ->

                    let user = User.create (name, 0uL)
                    let id = User.save_new (user)

                    do!
                        reply (
                            sprintf
                                "Created user '%s' with id '%i'\n\nAuth token:\n`%s`"
                                user.Username
                                id
                                user.AuthToken
                        )

            | "reloadbackbeat" ->
                Backbeat.init ()
                do! reply_emoji ":white_check_mark:"

            | "addleaderboard" ->
                match args with
                | [] ->
                    do!
                        reply
                            "Enter a hash and ruleset, for example: $addleaderboard 1467CD6DEB4A3B87FA58FAB4F2398BE9AD7B0017031C511C549D3EF28FFB58D3"
                | hash :: _ ->
                    match Backbeat.Charts.by_hash hash with
                    | None -> do! reply "Chart not found."
                    | Some(chart, song) ->

                    Leaderboard.create hash "SC(J4)548E5A"

                    let msg =
                        sprintf
                            "## :checkered_flag: Leaderboard created:\n%s [%s]"
                            song.FormattedTitle
                            chart.DifficultyName

                    let! _ = (client.GetChannel(FEED_CHANNEL_ID) :?> SocketTextChannel).SendMessageAsync(msg)
                    do! reply_emoji ":white_check_mark:"

            | "addleaderboards" ->
                let missing = ResizeArray<string>()

                for table in Backbeat.tables.Values do
                    for level in table.Levels do
                        for chart in level.Charts do
                            match Backbeat.Charts.by_hash chart.Hash with
                            | None -> missing.Add chart.Id
                            | Some(_, _) ->
                                if not (Leaderboard.exists chart.Hash table.RulesetId) then
                                    Leaderboard.create chart.Hash table.RulesetId

                    if missing.Count > 0 then
                        do! reply ("Missing charts:\n  " + String.concat "\n  " missing)

                    do! reply_emoji ":white_check_mark:"

            | _ -> ()
        }

    let interaction
        (client: DiscordSocketClient)
        (user_id: int64, user_info: User)
        (context: SocketMessageComponent)
        (command: string)
        (args: string list)
        =
        task {
            match command with
            | "users" ->
                match args with
                | page :: [] ->
                    let embed, components = user_list (int page)

                    do!
                        context.UpdateAsync(fun msg ->
                            msg.Embed <- embed
                            msg.Components <- components
                        )
                | _ -> failwith "impossible"
            | _ -> ()
        }
