namespace Interlude.Web.Server.Bot

open System
open System.Threading
open Discord
open Discord.WebSocket
open Percyqaz.Common
open Interlude.Web.Server
open Interlude.Web.Server.Domain

module Bot =

    let on_log(msg: LogMessage) = task { Logging.Debug(msg.Message) }

    let on_ready() = task { Logging.Info("Bot is ready") }

    let on_message(message: SocketMessage) = 
        task {
            // Normal user commands via #bot
            // Require a registered account
            if message.Channel.Id = MAIN_CHANNEL_ID && message.Content.StartsWith "$" then
                let cmd = message.Content.Substring(1).Split(' ', 2, StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
                match User.by_discord_id(message.Author.Id) with
                | Some (id, user) ->
                    do! 
                        Commands.user_dispatch
                            (id, user)
                            message
                            cmd.[0]
                            (
                                if cmd.Length > 1 then 
                                    cmd.[1].Split("$", StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries) |> List.ofArray 
                                else []
                            )
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
                    do! 
                        Commands.admin_dispatch
                            (id, user)
                            message
                            cmd.[0]
                            (
                                if cmd.Length > 1 then 
                                    cmd.[1].Split("$", StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries) |> List.ofArray 
                                else []
                            )
                | _ ->
                    Logging.Warn(sprintf "Discord user with id %i attempted to trigger an admin command" message.Author.Id)
                    do! message.AddReactionAsync(Emoji.Parse(":skull:"))
        }

    let on_interaction_created(interaction: SocketInteraction) =
        task {
            match interaction with
            | :? SocketMessageComponent as s -> Logging.Info(s.Data.CustomId)
            | _ -> ()
        }

    let start() =
        try
            //Charts.init()
            Logging.Info(sprintf "Backbeat init complete (it's commented out), %i Charts and %i Songs" Charts.charts.Count Charts.songs.Count)
            let config = DiscordSocketConfig(GatewayIntents = (GatewayIntents.MessageContent ||| GatewayIntents.AllUnprivileged ^^^ GatewayIntents.GuildInvites ^^^ GatewayIntents.GuildScheduledEvents))
            use client = new DiscordSocketClient(config)
        
            client.add_Ready(fun _ -> on_ready())
            client.add_MessageReceived(fun msg -> on_message msg)
            client.add_Log(fun log -> on_log log)
            client.add_InteractionCreated(fun i -> on_interaction_created i)
        
            client.LoginAsync(TokenType.Bot, SECRETS.DiscordBotToken)
            |> Async.AwaitTask
            |> Async.RunSynchronously
        
            client.StartAsync()
            |> Async.AwaitTask
            |> Async.RunSynchronously
        
            Thread.Sleep Timeout.Infinite
        
        with err ->
            Logging.Critical (err.ToString(), err)