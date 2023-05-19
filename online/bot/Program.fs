open System
open System.Threading
open System.Reflection
open Discord
open Discord.WebSocket
open Percyqaz.Common
open Interlude.Web.Bot

Logging.LogFile <- None
Logging.Verbosity <- LoggingLevel.DEBUG

let BOT_TOKEN = 
    try System.IO.File.ReadAllText("secret.txt")
    with err ->
        Logging.Error("Couldn't get bot token", err)
        ""

let tagline = 
    let stream = Assembly.GetExecutingAssembly().GetManifestResourceStream("Interlude.Web.Bot.Version.txt")
    use tr = new IO.StreamReader(stream)
    tr.ReadToEnd()

let on_log(msg: LogMessage) =
    task {
        Logging.Debug(msg.Message)
    }

let on_ready() = 
    task {
        Logging.Info("Bot is ready")
    }

let on_message(message: SocketMessage) = 
    task {
        if message.Author.Id = PERCYQAZ_ID && message.Channel.Id = CHANNEL_ID && message.Content.StartsWith "$" then
            let cmd = message.Content.Substring(1).Split(' ', 2, StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
            do! Commands.dispatch message cmd.[0] (if cmd.Length > 1 then Some cmd.[1] else None)
    }

let on_interaction_created(interaction: SocketInteraction) =
    task {
        match interaction with
        | :? SocketMessageComponent as s -> Logging.Info(s.Data.CustomId)
        | _ -> ()
    }

try

    Charts.init()
    Logging.Info(sprintf "Backbeat init complete, %i Charts and %i Songs" Charts.charts.Count Charts.songs.Count)
    Logging.Info(sprintf "Launching bot [%s]" tagline)
    let config = DiscordSocketConfig(GatewayIntents = (GatewayIntents.MessageContent ||| GatewayIntents.AllUnprivileged ^^^ GatewayIntents.GuildInvites ^^^ GatewayIntents.GuildScheduledEvents))
    use client = new DiscordSocketClient(config)

    client.add_Ready(fun _ -> on_ready())
    client.add_MessageReceived(fun msg -> on_message msg)
    client.add_Log(fun log -> on_log log)
    client.add_InteractionCreated(fun i -> on_interaction_created i)

    client.LoginAsync(TokenType.Bot, BOT_TOKEN)
    |> Async.AwaitTask
    |> Async.RunSynchronously

    client.StartAsync()
    |> Async.AwaitTask
    |> Async.RunSynchronously

    Thread.Sleep Timeout.Infinite

with err ->
    Logging.Critical (err.ToString(), err)

Logging.Shutdown()