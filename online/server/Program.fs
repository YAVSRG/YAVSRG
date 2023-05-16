open System
open System.Threading
open System.Reflection
open Percyqaz.Common
open Interlude.Web.Shared
open Interlude.Web.Server

let handle_packet(id: Guid, packet: Upstream) =
    async {
        match packet with

        | Upstream.VERSION v -> 
            if v = PROTOCOL_VERSION then UserState.handshake id
            else Server.kick(id, "Your client is out of date")
        | Upstream.LOGIN username ->
            UserState.login(id, username)
        | Upstream.LOGOUT ->
            Lobby.ensure_player_leaves_lobby(id, fun () -> UserState.logout id)

        | Upstream.GET_LOBBIES -> Lobby.List(id).Do
        | Upstream.JOIN_LOBBY lid -> Lobby.Join(id, lid).Do
        | Upstream.CREATE_LOBBY name -> Lobby.Create(id, name).Do

        | Upstream.INVITE_TO_LOBBY username -> Lobby.Invite(id, username).Do
        | Upstream.LEAVE_LOBBY -> Lobby.Leave(id).Do
        | Upstream.CHAT msg -> Lobby.Chat(id, msg).Do
        | Upstream.READY_STATUS r -> Lobby.ReadyUp(id, r).Do
        | Upstream.MISSING_CHART -> Lobby.MissingChart(id).Do

        | Upstream.BEGIN_PLAYING -> Lobby.BeginPlaying(id).Do
        | Upstream.PLAY_DATA data -> Lobby.PlayData(id, data).Do
        | Upstream.BEGIN_SPECTATING -> Lobby.BeginSpectating(id).Do
        | Upstream.FINISH_PLAYING abandoned -> Lobby.FinishPlaying(id, abandoned).Do
        
        | Upstream.TRANSFER_HOST who -> Lobby.ChangeHost(id, who).Do
        | Upstream.SELECT_CHART c -> Lobby.SelectChart(id, c).Do
        | Upstream.LOBBY_SETTINGS s -> Lobby.Settings(id, s).Do
        | Upstream.START_GAME -> Lobby.StartGame(id).Do
        | Upstream.CANCEL_GAME -> Lobby.CancelStartGame(id).Do

        | Upstream.KICK_PLAYER _ -> Server.kick(id, "not yet implemented")

    } |> Async.Start

let handle_connect(id: Guid) = UserState.connect id
let handle_disconnect(id: Guid) = 
    Lobby.ensure_player_leaves_lobby (id, fun () -> UserState.disconnect id)

let PORT = 
    try Environment.GetEnvironmentVariable "PORT" |> int
    with err -> 32767

let tagline = 
    let stream = Assembly.GetExecutingAssembly().GetManifestResourceStream("Interlude.Web.Server.Version.txt")
    use tr = new IO.StreamReader(stream)
    tr.ReadToEnd()

try

    Logging.Info(sprintf "Launching server [%s] on port %i ..." tagline PORT)
    Server.init { 
        Address = "0.0.0.0"
        Port = PORT
        Handle_Packet = handle_packet
        Handle_Connect = handle_connect
        Handle_Disconnect = handle_disconnect
    }

    Server.start()

    Thread.Sleep Timeout.Infinite

with err ->
    Logging.Critical (err.ToString(), err)

Logging.Shutdown()