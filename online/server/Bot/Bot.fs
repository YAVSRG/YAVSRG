namespace Interlude.Web.Server.Bot

open System
open System.Threading
open Discord
open Discord.WebSocket
open Percyqaz.Common
open Interlude.Web.Server
open Interlude.Web.Server.Domain

module Bot =

    let config = DiscordSocketConfig(GatewayIntents = (GatewayIntents.MessageContent ||| GatewayIntents.AllUnprivileged ^^^ GatewayIntents.GuildInvites ^^^ GatewayIntents.GuildScheduledEvents))
    let client = new DiscordSocketClient(config)

    let on_log(msg: LogMessage) = task { Logging.Debug("[BOT] " + msg.Message) }

    let on_message(message: SocketMessage) = 
        task {
            // Normal user commands via #bot
            // Require a registered account
            if message.Channel.Id = MAIN_CHANNEL_ID && message.Content.StartsWith "$" then
                let cmd = message.Content.Substring(1).Split(' ', 2, StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
                match User.by_discord_id(message.Author.Id) with
                | Some (id, user) ->
                    try
                        do! 
                            UserCommands.dispatch
                                client
                                (id, user)
                                message
                                (cmd.[0].ToLower())
                                (
                                    if cmd.Length > 1 then 
                                        cmd.[1].Split("$", StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries) |> List.ofArray 
                                    else []
                                )
                    with err ->
                        Logging.Error(sprintf "Error handling user command '%s': %O" message.Content err)
                        do! message.AddReactionAsync(Emoji.Parse(":alien:"))
                | None -> do! message.AddReactionAsync(Emoji.Parse(":no_entry_sign:"))

            // Bootstrap command to give me the 'developer' badge
            elif message.Channel.Id = ADMIN_CHANNEL_ID && message.Author.Id = PERCYQAZ_ID && message.Content.StartsWith("$developer") then
                match User.by_discord_id(PERCYQAZ_ID) with
                | Some (id, user) ->
                    User.save(id, { user with Badges = Set.add Badge.DEVELOPER user.Badges })
                    do! message.AddReactionAsync(Emoji.Parse(":heart_eyes:"))
                | None -> do! message.AddReactionAsync(Emoji.Parse(":face_with_spiral_eyes:"))

            // Admin dashboard commands via #admin, hidden channel for admins
            // Require a registered account with the 'developer' badge
            elif message.Channel.Id = ADMIN_CHANNEL_ID && message.Content.StartsWith "$" then
                let cmd = message.Content.Substring(1).Split(' ', 2, StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
                match User.by_discord_id(message.Author.Id) with
                | Some (id, user) when user.Badges.Contains(Badge.DEVELOPER) -> 
                    try
                        do! 
                            AdminCommands.dispatch
                                client
                                (id, user)
                                message
                                (cmd.[0].ToLower())
                                (
                                    if cmd.Length > 1 then 
                                        cmd.[1].Split("$", StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries) |> List.ofArray 
                                    else []
                                )
                    with err ->
                        Logging.Error(sprintf "Error handling admin command '%s': %O" message.Content err)
                        do! message.AddReactionAsync(Emoji.Parse(":alien:"))
                | _ ->
                    Logging.Warn(sprintf "Discord user with id %i attempted to trigger an admin command" message.Author.Id)
                    do! message.AddReactionAsync(Emoji.Parse(":skull:"))
        }

    let on_button_executed(comp: SocketMessageComponent) =
        task {
            if comp.Channel.Id = ADMIN_CHANNEL_ID || comp.Channel.Id = MAIN_CHANNEL_ID then
                let cmd = comp.Data.CustomId.Split(' ', 2, StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
                match User.by_discord_id(comp.User.Id) with
                | Some (id, user) ->
                    try
                        do! 
                            AdminCommands.interaction
                                client
                                (id, user)
                                comp
                                (cmd.[0].ToLower())
                                (
                                    if cmd.Length > 1 then 
                                        cmd.[1].Split("$", StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries) |> List.ofArray 
                                    else []
                                )
                    with err ->
                        Logging.Error(sprintf "Error handling button click '%s': %O" comp.Data.CustomId err)
                        do! comp.Message.AddReactionAsync(Emoji.Parse(":alien:"))
                | None -> ()
        }

    let start() =
        try

            let mutable startup_message_shown = false
        
            client.add_Ready(fun () -> 
                task {
                    if not startup_message_shown then
                        let! _ = (client.GetChannel(ADMIN_CHANNEL_ID) :?> SocketTextChannel).SendMessageAsync(sprintf "I've just restarted! Running \"%s\"" TAGLINE)
                        startup_message_shown <- true
                    return ()
                })
            client.add_MessageReceived(fun msg -> on_message msg)
            //client.add_Log(fun log -> on_log log)

            client.add_ButtonExecuted(fun x -> on_button_executed x)
        
            client.LoginAsync(TokenType.Bot, SECRETS.DiscordBotToken)
            |> Async.AwaitTask
            |> Async.RunSynchronously
        
            client.StartAsync()
            |> Async.AwaitTask
            |> Async.RunSynchronously
        
            Thread.Sleep Timeout.Infinite
        
        with err ->
            Logging.Critical (err.ToString(), err)