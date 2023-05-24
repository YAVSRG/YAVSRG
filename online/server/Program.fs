open System
open System.IO
open System.Threading
open System.Security.Authentication
open System.Security.Cryptography.X509Certificates
open System.Reflection
open Percyqaz.Common
open Percyqaz.Json
open Interlude.Web.Shared
open Interlude.Web.Server

[<Json.AutoCodec(false)>]
type Secrets = 
    {
        SocketCert: string
        SocketCertPassword: string
        ApiCert: string
        ApiCertPassword: string
        BotToken: string
    }
    static member Default = {
            SocketCert = "localhost.pfx"
            SocketCertPassword = "DEVELOPMENT"
            ApiCert = "localhost.pfx"
            ApiCertPassword = "DEVELOPMENT"
            BotToken = ""
        }

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

let SOCKET_PORT = 32767
let HTTPS_PORT = 443

let tagline = 
    let stream = Assembly.GetExecutingAssembly().GetManifestResourceStream("Interlude.Web.Server.Version.txt")
    use tr = new StreamReader(stream)
    tr.ReadToEnd()

try
    let secrets =
        if not (Directory.Exists "./secrets") then
            #if DEBUG
                failwith "Secrets folder not found! You are running the server as a non-docker instance, make sure you ran it with the script"
            #else
                failwith "Secrets folder not found! Did you mount it properly?"
            #endif
        match Prelude.Common.JSON.FromFile<Secrets>("./secrets/secrets.json") with
        | Ok o -> o
        | Error e ->
            failwithf "Error while reading secrets.json: %O" e

    let api_cert = new X509Certificate2(Path.Combine("./secrets", secrets.ApiCert), secrets.ApiCertPassword)
    let socket_cert = new X509Certificate2(Path.Combine("./secrets", secrets.SocketCert), secrets.SocketCertPassword)

    Logging.Info(sprintf "Interlude.Web ~~ %s" tagline)

    Server.init { 
        Address = "0.0.0.0"
        Port = SOCKET_PORT
        Handle_Packet = handle_packet
        Handle_Connect = handle_connect
        Handle_Disconnect = handle_disconnect
    }
    
    API.init {
        Port = HTTPS_PORT
        SSLContext = NetCoreServer.SslContext(SslProtocols.Tls12, api_cert)
    }

    Logging.Info(sprintf "Launching game server on port %i ..." SOCKET_PORT)
    Server.start()
    Logging.Info(sprintf "Launching api on port %i ..." HTTPS_PORT)
    API.start()

    Thread.Sleep Timeout.Infinite

with err ->
    Logging.Critical (err.ToString(), err)
    Console.ReadLine() |> ignore

Logging.Shutdown()