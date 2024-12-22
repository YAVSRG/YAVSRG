namespace Interlude.Web.Server.Bot

open System
open System.Threading
open Discord
open Discord.WebSocket
open Percyqaz.Common
open Interlude.Web.Server
open Interlude.Web.Server.Domain.Core

module Bot =

    let config =
        DiscordSocketConfig(
            GatewayIntents =
                (GatewayIntents.MessageContent
                 ||| GatewayIntents.AllUnprivileged
                     ^^^ GatewayIntents.GuildInvites
                     ^^^ GatewayIntents.GuildScheduledEvents)
        )

    let client = new DiscordSocketClient(config)

    let on_log (msg: LogMessage) =
        task { Logging.Debug "[BOT] %s" msg.Message }

    let on_message (message: SocketMessage) =
        task {
            // Normal user commands via #bot
            // Require a registered account
            if message.Channel.Id = MAIN_CHANNEL_ID && message.Content.StartsWith "$" then
                let cmd =
                    message.Content
                        .Substring(1)
                        .Split(' ', 2, StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)

                match User.by_discord_id (message.Author.Id) with
                | Some(id, user) ->
                    try
                        do!
                            UserCommands.dispatch
                                client
                                (id, user)
                                message
                                (cmd.[0].ToLower())
                                (if cmd.Length > 1 then
                                     cmd.[1]
                                         .Split(
                                             "$",
                                             StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries
                                         )
                                     |> List.ofArray
                                 else
                                     [])
                    with err ->
                        Logging.Error "Error handling user command '%s': %O" message.Content err
                        do! message.AddReactionAsync(Emoji.Parse(":alien:"))
                | None -> do! message.AddReactionAsync(Emoji.Parse(":no_entry_sign:"))

            // Bootstrap command to give me the 'developer' badge
            elif
                message.Channel.Id = ADMIN_CHANNEL_ID
                && message.Author.Id = PERCYQAZ_ID
                && message.Content.StartsWith("$developer")
            then
                match User.by_discord_id (PERCYQAZ_ID) with
                | Some(id, user) ->
                    User.update_badges (id, Set.add Badge.DEVELOPER user.Badges)

                    do! message.AddReactionAsync(Emoji.Parse(":heart_eyes:"))
                | None -> do! message.AddReactionAsync(Emoji.Parse(":face_with_spiral_eyes:"))

            // Admin dashboard commands via #admin, hidden channel for admins
            // Require a registered account with the 'developer' badge
            elif message.Channel.Id = ADMIN_CHANNEL_ID && message.Content.StartsWith "$" then
                let cmd =
                    message.Content
                        .Substring(1)
                        .Split(' ', 2, StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)

                match User.by_discord_id (message.Author.Id) with
                | Some(id, user) when user.Badges.Contains(Badge.DEVELOPER) ->
                    try
                        do!
                            AdminCommands.dispatch
                                client
                                (id, user)
                                message
                                (cmd.[0].ToLower())
                                (if cmd.Length > 1 then
                                     cmd.[1]
                                         .Split(
                                             "$",
                                             StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries
                                         )
                                     |> List.ofArray
                                 else
                                     [])
                    with err ->
                        Logging.Error "Error handling admin command '%s': %O" message.Content err
                        do! message.AddReactionAsync(Emoji.Parse(":alien:"))
                | _ ->
                    Logging.Warn
                        "Discord user with id %i attempted to trigger an admin command"
                        message.Author.Id

                    do! message.AddReactionAsync(Emoji.Parse(":skull:"))
        }

    let on_button_executed (comp: SocketMessageComponent) =
        task {
            let cmd =
                comp.Data.CustomId.Split(
                    ' ',
                    2,
                    StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries
                )
            if comp.Channel.Id = ADMIN_CHANNEL_ID then
                match User.by_discord_id (comp.User.Id) with
                | Some(id, user) when user.Badges.Contains(Badge.DEVELOPER) ->
                    try
                        do!
                            AdminInteractables.handle_interaction
                                client
                                (id, user)
                                comp
                                (cmd.[0].ToLower())
                                (if cmd.Length > 1 then
                                     cmd.[1]
                                         .Split(
                                             "$",
                                             StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries
                                         )
                                     |> List.ofArray
                                 else
                                     [])
                    with err ->
                        Logging.Error "Error handling button click '%s': %O" comp.Data.CustomId err
                        do! comp.Message.AddReactionAsync(Emoji.Parse(":alien:"))
                | _ -> ()
            elif comp.Channel.Id = MAIN_CHANNEL_ID then
                match User.by_discord_id (comp.User.Id) with
                | Some(id, user) ->
                    try
                        do!
                            UserInteractables.handle_interaction
                                client
                                (id, user)
                                comp
                                (cmd.[0].ToLower())
                                (if cmd.Length > 1 then
                                     cmd.[1]
                                         .Split(
                                             "$",
                                             StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries
                                         )
                                     |> List.ofArray
                                 else
                                     [])
                    with err ->
                        Logging.Error "Error handling button click '%s': %O" comp.Data.CustomId err
                        do! comp.Message.AddReactionAsync(Emoji.Parse(":alien:"))
                | None -> ()
        }

    let start () =
        try

            let mutable startup_message_shown = false

            client.add_Ready (fun () ->
                task {
                    Discord.debug_log <-
                        fun s ->
                            let s =
                                if s.Length > 2000 then
                                    s.Substring(0, 1995) + "\n..."
                                else
                                    s

                            try
                                (client.GetChannel(ADMIN_CHANNEL_ID) :?> SocketTextChannel).SendMessageAsync(s)
                                |> Async.AwaitTask
                                |> Async.Ignore
                                |> Async.RunSynchronously
                            with err ->
                                Logging.Critical "Exception while trying to debug-log message '%s': %O" s err

                    Discord.feed_log <-
                        fun s ->
                            let s =
                                if s.Length > 2000 then
                                    s.Substring(0, 1995) + "\n..."
                                else
                                    s

                            try
                                (client.GetChannel(FEED_CHANNEL_ID) :?> SocketTextChannel).SendMessageAsync(s)
                                |> Async.AwaitTask
                                |> Async.Ignore
                                |> Async.RunSynchronously
                            with err ->
                                Logging.Critical "Exception while trying to post feed message '%s': %O" s err

                    if not startup_message_shown then

                        if SECRETS.IsProduction then
                            sprintf "I've just been deployed! Running \"%s\"" TAGLINE
                        else
                            "Test server has restarted :)"
                        |> Discord.debug_log

                        startup_message_shown <- true

                    return ()
                }
            )

            client.add_MessageReceived (fun msg -> on_message msg)
            //client.add_Log(fun log -> on_log log)

            client.add_ButtonExecuted (fun x -> on_button_executed x)

            client.LoginAsync(TokenType.Bot, SECRETS.DiscordBotToken)
            |> Async.AwaitTask
            |> Async.RunSynchronously

            client.StartAsync() |> Async.AwaitTask |> Async.RunSynchronously

            Thread.Sleep Timeout.Infinite

        with err ->
            Logging.Critical "%O" err

    let create_admin_prompt (embed, components) : unit =
        (client.GetChannel(ADMIN_CHANNEL_ID) :?> SocketTextChannel).SendMessageAsync(embed = embed, components = components)
        |> Async.AwaitTask
        |> Async.Ignore
        |> Async.RunSynchronously