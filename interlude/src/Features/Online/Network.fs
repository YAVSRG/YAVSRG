namespace Interlude.Features.Online

open System
open System.Net
open System.IO
open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Interlude.UI
open Interlude.Web.Shared

module NetworkEvents =

    let waiting_registration_ev = new Event<string>()
    let waiting_registration = waiting_registration_ev.Publish

    let login_failed_ev = new Event<string>()
    let login_failed = login_failed_ev.Publish

    let registration_failed_ev = new Event<string>()
    let registration_failed = registration_failed_ev.Publish

    let successful_login_ev = new Event<string>()
    let successful_login = successful_login_ev.Publish

    let receive_lobby_list_ev = new Event<LobbyInfo array>()
    let receive_lobby_list = receive_lobby_list_ev.Publish

    let receive_invite_ev = new Event<string * Guid>()
    let receive_invite = receive_invite_ev.Publish

    let join_lobby_ev = new Event<Lobby>()
    let join_lobby = join_lobby_ev.Publish

    let leave_lobby_ev = new Event<unit>()
    let leave_lobby = leave_lobby_ev.Publish

module Network =

    type Status =
        | NotConnected
        | Connecting
        | ConnectionFailed
        | Connected
        | LoggedIn

    let mutable status = NotConnected

    let mutable lobby_invites: LobbyInvite list = []
    let mutable lobby: Lobby option = None

    let credentials = Credentials.Load()

    let private target_ip =
        try
            Dns.GetHostAddresses(credentials.Host).[0]
        with err ->
            Logging.Error("Failed to perform DNS lookup for " + credentials.Host, err)
            IPAddress.Parse("0.0.0.0")

    type NetworkClient() =
        inherit Client(target_ip, 32767)

        override this.OnConnected() = 
            sync <| fun () -> status <- Connected

        override this.OnDisconnected() =
            sync 
            <| fun () -> 
                status <-
                    if status = Connecting then
                        ConnectionFailed
                    else
                        NotConnected

                if lobby.IsSome then
                    lobby <- None
                    NetworkEvents.leave_lobby_ev.Trigger()

        override this.OnPacketReceived(packet: Downstream) =
            match packet with
            | Downstream.DISCONNECT reason ->
                Logging.Info(sprintf "Disconnected from server: %s" reason)

                sync
                <| fun () -> Notifications.error (%"notification.network.disconnected", reason)
            | Downstream.HANDSHAKE_SUCCESS ->
                if credentials.Token <> "" then
                    this.Send(Upstream.LOGIN credentials.Token)
            | Downstream.DISCORD_AUTH_URL url ->
                if not (url.StartsWith("https://discord.com/api/oauth2")) then
                    Logging.Error(sprintf "Got a strange auth link! %s" url)
                else
                    open_url url
            | Downstream.COMPLETE_REGISTRATION_WITH_DISCORD discord_tag ->
                Logging.Debug("Linking an account with: " + discord_tag)
                sync <| fun () -> NetworkEvents.waiting_registration_ev.Trigger discord_tag
            | Downstream.REGISTRATION_FAILED reason ->
                Logging.Info(sprintf "Registration failed: %s" reason)
                Notifications.error (%"notification.network.registrationfailed", reason)
                sync <| fun () -> NetworkEvents.registration_failed_ev.Trigger reason
            | Downstream.AUTH_TOKEN token ->
                credentials.Token <- token
                this.Send(Upstream.LOGIN credentials.Token)
            | Downstream.LOGIN_SUCCESS name ->
                Logging.Info(sprintf "Logged in as %s" name)

                sync
                <| fun () ->
                    credentials.Username <- name
                    status <- LoggedIn

                    API.Client.authenticate credentials.Token

                    if Screen.current_type <> Screen.Type.SplashScreen then
                        Notifications.system_feedback (Icons.GLOBE, [ name ] %> "notification.network.login", "")

                    NetworkEvents.successful_login_ev.Trigger name
            | Downstream.LOGIN_FAILED reason ->
                Logging.Info(sprintf "Login failed: %s" reason)
                credentials.Token <- ""

                sync
                <| fun () ->
                    if Screen.current_type <> Screen.Type.SplashScreen then
                        Notifications.error (%"notification.network.loginfailed", reason)

                    NetworkEvents.login_failed_ev.Trigger reason

            | Downstream.LOBBY_LIST lobbies ->
                sync
                <| fun () -> 
                    NetworkEvents.receive_lobby_list_ev.Trigger lobbies
            | Downstream.YOU_JOINED_LOBBY (players: (string * int32) array) ->
                sync
                <| fun () ->
                    lobby <- Some <| Lobby(this, players)
                    NetworkEvents.join_lobby_ev.Trigger(lobby.Value)
            | Downstream.INVITED_TO_LOBBY(by_user, lobby_id) ->
                sync 
                <| fun () ->
                    lobby_invites <- { InvitedBy = by_user; LobbyId = lobby_id; } :: lobby_invites
                    NetworkEvents.receive_invite_ev.Trigger(by_user, lobby_id)

            | Downstream.YOU_LEFT_LOBBY ->
                sync
                <| fun () ->
                    lobby <- None
                    NetworkEvents.leave_lobby_ev.Trigger()
            | Downstream.YOU_ARE_HOST b -> 
                sync 
                <| fun () ->
                    lobby.Value.YouAreHost <- b
            | Downstream.PLAYER_JOINED_LOBBY(username, color) ->
                sync
                <| fun () ->
                    match lobby with
                    | None -> Logging.Debug(sprintf "Unexpected lobby packet: %A" packet)
                    | Some lobby -> lobby.PlayerJoined(username, color)
            | Downstream.PLAYER_LEFT_LOBBY username ->
                sync
                <| fun () ->
                    match lobby with
                    | None -> Logging.Debug(sprintf "Unexpected lobby packet: %A" packet)
                    | Some lobby -> lobby.PlayerLeft(username)
            | Downstream.SELECT_CHART lc ->
                sync
                <| fun () ->
                    match lobby with
                    | None -> Logging.Debug(sprintf "Unexpected lobby packet: %A" packet)
                    | Some lobby -> lobby.ChartSelected(lc)
            | Downstream.LOBBY_SETTINGS settings ->
                sync
                <| fun () ->
                    match lobby with
                    | None -> Logging.Debug(sprintf "Unexpected lobby packet: %A" packet)
                    | Some lobby -> lobby.UpdateSettings(settings)
            | Downstream.LOBBY_EVENT(kind, data) -> 
                sync 
                <| fun () -> 
                    match lobby with
                    | None -> Logging.Debug(sprintf "Unexpected lobby packet: %A" packet)
                    | Some lobby -> lobby.LobbyEvent(kind, data)
            | Downstream.SYSTEM_MESSAGE msg ->
                Logging.Info(sprintf "[NETWORK] %s" msg)
                sync 
                <| fun () -> 
                    match lobby with
                    | None -> Logging.Debug(sprintf "Unexpected lobby packet: %A" packet)
                    | Some lobby -> lobby.SystemMessage(msg)
            | Downstream.CHAT(sender, msg) ->
                sync 
                <| fun () -> 
                    match lobby with
                    | None -> Logging.Debug(sprintf "Unexpected lobby packet: %A" packet)
                    | Some lobby -> lobby.ChatMessage(sender, msg)
            | Downstream.PLAYER_STATUS(username, status) ->
                sync
                <| fun () ->
                    match lobby with
                    | None -> Logging.Debug(sprintf "Unexpected lobby packet: %A" packet)
                    | Some lobby -> lobby.PlayerStatus(username, status)
            | Downstream.COUNTDOWN(reason, seconds) ->
                sync 
                <| fun () -> 
                    match lobby with
                    | None -> Logging.Debug(sprintf "Unexpected lobby packet: %A" packet)
                    | Some lobby -> lobby.StartCountdown(reason, seconds)

            | Downstream.GAME_COUNTDOWN b -> 
                sync 
                <| fun () -> 
                    match lobby with
                    | None -> Logging.Debug(sprintf "Unexpected lobby packet: %A" packet)
                    | Some lobby -> lobby.Countdown <- b
            | Downstream.GAME_START ->
                sync 
                <| fun () -> 
                    match lobby with
                    | None -> Logging.Debug(sprintf "Unexpected lobby packet: %A" packet)
                    | Some lobby -> lobby.GameStart()
            | Downstream.GAME_END ->
                sync 
                <| fun () -> 
                    match lobby with
                    | None -> Logging.Debug(sprintf "Unexpected lobby packet: %A" packet)
                    | Some lobby -> lobby.GameEnd()
            | Downstream.PLAY_DATA(username, data) ->
                sync 
                <| fun () -> 
                    match lobby with
                    | None -> Logging.Debug(sprintf "Unexpected lobby packet: %A" packet)
                    | Some lobby -> 
                        match lobby.GetReplayInfo username with
                        | None -> Logging.Debug(sprintf "Unexpected replay data for %s" username)
                        | Some replay_info ->
                            use ms = new MemoryStream(data)
                            use br = new BinaryReader(ms)
                            (replay_info.Replay :?> Gameplay.OnlineReplayProvider).ImportLiveBlock br

    let client = new NetworkClient()

    let mutable private api_initialised = false
    let connect () =
        if not api_initialised then
            API.Client.init ("https://" + credentials.Api)
            api_initialised <- true

        if status <> NotConnected && status <> ConnectionFailed then
            ()
        else
            status <- Connecting

            Logging.Info(sprintf "Connecting to %s ..." credentials.Host)
            client.Connect()

    let login_with_token () =
        client.Send(Upstream.LOGIN credentials.Token)

    let begin_login () =
        client.Send(Upstream.BEGIN_LOGIN_WITH_DISCORD)

    let begin_registration () =
        client.Send(Upstream.BEGIN_REGISTRATION_WITH_DISCORD)

    let complete_registration (desired_username) =
        client.Send(Upstream.COMPLETE_REGISTRATION_WITH_DISCORD desired_username)

    let logout () =
        NetworkEvents.leave_lobby_ev.Trigger()
        lobby <- None

        if status = LoggedIn then
            client.Send Upstream.LOGOUT
            status <- Connected
            credentials.Token <- ""

    let disconnect () =
        if lobby.IsSome then
            sync NetworkEvents.leave_lobby_ev.Trigger

        lobby <- None
        client.Disconnect()

    let init_window () =
        if target_ip.ToString() <> "0.0.0.0" then
            connect ()

    let deinit () =
        if status <> NotConnected then
            client.Disconnect()

        credentials.Save()

    let join_lobby id = client.Send(Upstream.JOIN_LOBBY id)