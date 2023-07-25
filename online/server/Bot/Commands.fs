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
    let user_dispatch (userId: int64, userInfo: User) (context: SocketMessage) (command: string) (args: string list) =
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
            match command with
            | "s"
            | "search" ->
                match args with

                | [] -> 
                    do! reply "Enter a search term, for example: search PLANET//SHAPER artist:Camellia creator:Evening"

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
            | _ -> ()
        }

    // Requires user to exist, with the developer role
    let admin_dispatch (userId: int64, userInfo: User) (context: SocketMessage) (command: string) (args: string list) =
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
                        User.delete(id)
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
            | _ -> ()
        }