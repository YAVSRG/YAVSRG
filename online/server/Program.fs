open System
open System.Threading
open System.Reflection
open Percyqaz.Common
open Interlude.Web.Shared
open Interlude.Web.Server

let log_packet(id: Guid, packet: Upstream) =
    match packet with
    | Upstream.PLAY_DATA _ -> ()
    | _ -> Logging.Debug (sprintf "%O >> %A" id packet)
let handle_packet(id: Guid, packet: Upstream) =
    async {
        log_packet (id, packet)
        match packet with

        | Upstream.VERSION v -> 
            if v = PROTOCOL_VERSION then UserState.handshake id
            else Server.kick(id, "Protocol version mismatch")
        | Upstream.LOGIN username ->
            UserState.login(id, username)
        | Upstream.LOGOUT ->
            Lobby.user_disconnected(id, fun () -> UserState.logout id)

        | Upstream.GET_LOBBIES -> Lobby.list id
        | Upstream.JOIN_LOBBY lid -> Lobby.join (id, lid)
        | Upstream.CREATE_LOBBY name -> Lobby.create (id, name)

        | Upstream.INVITE_TO_LOBBY username -> Lobby.invite (id, username)
        | Upstream.LEAVE_LOBBY -> Lobby.leave id
        | Upstream.CHAT msg -> Lobby.chat (id, msg)
        | Upstream.READY_STATUS r -> Lobby.ready_up (id, r)

        | Upstream.BEGIN_PLAYING -> Lobby.begin_playing id
        | Upstream.PLAY_DATA data -> Lobby.play_data (id, data)
        | Upstream.BEGIN_SPECTATING -> Lobby.begin_spectating id
        | Upstream.FINISH_PLAYING early_quit -> Lobby.finish_playing (id, early_quit)
        
        | Upstream.TRANSFER_HOST who -> Lobby.change_host (id, who)
        | Upstream.SELECT_CHART c -> Lobby.select_chart (id, c)
        | Upstream.LOBBY_SETTINGS s -> Lobby.settings (id, s)
        | Upstream.START_GAME -> Lobby.start_game id

        | _ -> Server.kick(id, "Not yet implemented")

    } |> Async.Start

let handle_connect(id: Guid) = UserState.connect id
let handle_disconnect(id: Guid) = 
    Lobby.user_disconnected (id, fun () -> UserState.disconnect id)

let PORT = 
    try Environment.GetEnvironmentVariable("PORT") |> int
    with err -> 32767

let tagline = 
    let stream = Assembly.GetExecutingAssembly().GetManifestResourceStream("Interlude.Web.Server.Version.txt")
    use tr = new IO.StreamReader(stream)
    tr.ReadToEnd()

try

    Logging.Info(sprintf "Launching server [%s] on port %i ..." tagline PORT)
    Server.init { 
        Address = "0.0.0.0"; Port = PORT;
        Handle_Packet = handle_packet
        Handle_Connect = handle_connect
        Handle_Disconnect = handle_disconnect
    }

    Server.start()

    Thread.Sleep Timeout.Infinite

with err ->
    Logging.Critical (err.ToString(), err)

Logging.Wait()