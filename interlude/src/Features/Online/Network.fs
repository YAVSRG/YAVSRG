namespace Interlude.Features.Online

open System
open System.Net
open System.IO
open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Prelude
open Prelude.Gameplay.Replays
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

    let mutable kicked_no_reconnect = false
    let mutable status = NotConnected

    let mutable lobby_invites: LobbyInvite list = []
    let mutable lobby: Lobby option = None

    let credentials = Credentials.Load()

    let private target_ip =
        try
            Dns.GetHostAddresses(credentials.Host).[0]
        with err ->
            Logging.Error "Failed to perform DNS lookup for %s: %O" credentials.Host err
            IPAddress.Parse("0.0.0.0")

    type NetworkClient() =
        inherit Client(target_ip, 32767)

        override this.OnConnected() =
            GameThread.defer <| fun () -> status <- Connected

        override this.OnDisconnected() =
            GameThread.defer
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
                Logging.Info "Disconnected from server: %s" reason
                kicked_no_reconnect <- true
                GameThread.defer
                <| fun () -> Notifications.error (%"notification.network.disconnected", reason)
            | Downstream.HANDSHAKE_SUCCESS ->
                if credentials.Token <> "" then
                    this.Send(Upstream.LOGIN credentials.Token)
            | Downstream.DISCORD_AUTH_URL url ->
                if not (url.StartsWith("https://discord.com/api/oauth2")) then
                    Logging.Error "Got a strange auth link! %s" url
                else
                    open_url url
            | Downstream.COMPLETE_REGISTRATION_WITH_DISCORD discord_tag ->
                Logging.Debug "Linking an account with: %s" discord_tag
                GameThread.defer <| fun () -> NetworkEvents.waiting_registration_ev.Trigger discord_tag
            | Downstream.REGISTRATION_FAILED reason ->
                Logging.Info "Registration failed: %s" reason
                Notifications.error (%"notification.network.registrationfailed", reason)
                GameThread.defer <| fun () -> NetworkEvents.registration_failed_ev.Trigger reason
            | Downstream.AUTH_TOKEN token ->
                credentials.Token <- token
                this.Send(Upstream.LOGIN credentials.Token)
            | Downstream.LOGIN_SUCCESS name ->
                Logging.Info "Logged in as %s" name

                GameThread.defer
                <| fun () ->
                    credentials.Username <- name
                    status <- LoggedIn

                    API.Client.authenticate credentials.Token

                    if Screen.current_type <> ScreenType.SplashScreen then
                        Notifications.system_feedback (Icons.GLOBE, [ name ] %> "notification.network.login", "")

                    NetworkEvents.successful_login_ev.Trigger name
            | Downstream.LOGIN_FAILED reason ->
                Logging.Info "Login failed: %s" reason
                credentials.Token <- ""

                GameThread.defer
                <| fun () ->
                    if Screen.current_type <> ScreenType.SplashScreen then
                        Notifications.error (%"notification.network.loginfailed", reason)

                    NetworkEvents.login_failed_ev.Trigger reason

            | Downstream.LOBBY_LIST lobbies ->
                GameThread.defer
                <| fun () ->
                    NetworkEvents.receive_lobby_list_ev.Trigger lobbies
            | Downstream.YOU_JOINED_LOBBY (players: (string * int32) array) ->
                GameThread.defer
                <| fun () ->
                    lobby <- Some <| Lobby(this, credentials.Username, players)
                    NetworkEvents.join_lobby_ev.Trigger(lobby.Value)
            | Downstream.INVITED_TO_LOBBY(by_user, lobby_id) ->
                GameThread.defer
                <| fun () ->
                    lobby_invites <- { InvitedBy = by_user; LobbyId = lobby_id; } :: lobby_invites
                    NetworkEvents.receive_invite_ev.Trigger(by_user, lobby_id)

            | Downstream.YOU_LEFT_LOBBY ->
                GameThread.defer
                <| fun () ->
                    lobby <- None
                    NetworkEvents.leave_lobby_ev.Trigger()
            | Downstream.YOU_ARE_HOST b ->
                GameThread.defer
                <| fun () ->
                    lobby.Value.YouAreHost <- b
            | Downstream.PLAYER_JOINED_LOBBY(username, color) ->
                GameThread.defer
                <| fun () ->
                    match lobby with
                    | None -> Logging.Debug "Unexpected lobby packet: %A" packet
                    | Some lobby -> lobby.PlayerJoined(username, color)
            | Downstream.PLAYER_LEFT_LOBBY username ->
                GameThread.defer
                <| fun () ->
                    match lobby with
                    | None -> Logging.Debug "Unexpected lobby packet: %A" packet
                    | Some lobby -> lobby.PlayerLeft(username)
            | Downstream.SELECT_CHART lc ->
                GameThread.defer
                <| fun () ->
                    match lobby with
                    | None -> Logging.Debug "Unexpected lobby packet: %A" packet
                    | Some lobby -> lobby.ChartSelected(lc)
            | Downstream.LOBBY_SETTINGS settings ->
                GameThread.defer
                <| fun () ->
                    match lobby with
                    | None -> Logging.Debug "Unexpected lobby packet: %A" packet
                    | Some lobby -> lobby.UpdateSettings(settings)
            | Downstream.LOBBY_EVENT(kind, data) ->
                GameThread.defer
                <| fun () ->
                    match lobby with
                    | None -> Logging.Debug "Unexpected lobby packet: %A" packet
                    | Some lobby -> lobby.LobbyEvent(kind, data)
            | Downstream.SYSTEM_MESSAGE msg ->
                Logging.Info "[NETWORK] %s" msg
                GameThread.defer
                <| fun () ->
                    match lobby with
                    | None -> Logging.Debug "Unexpected lobby packet: %A" packet
                    | Some lobby -> lobby.SystemMessage(msg)
            | Downstream.CHAT(sender, msg) ->
                GameThread.defer
                <| fun () ->
                    match lobby with
                    | None -> Logging.Debug "Unexpected lobby packet: %A" packet
                    | Some lobby -> lobby.ChatMessage(sender, msg)
            | Downstream.PLAYER_STATUS(username, status) ->
                GameThread.defer
                <| fun () ->
                    match lobby with
                    | None -> Logging.Debug "Unexpected lobby packet: %A" packet
                    | Some lobby -> lobby.PlayerStatus(username, status)
            | Downstream.COUNTDOWN(reason, seconds) ->
                GameThread.defer
                <| fun () ->
                    match lobby with
                    | None -> Logging.Debug "Unexpected lobby packet: %A" packet
                    | Some lobby -> lobby.StartCountdown(reason, seconds)

            | Downstream.GAME_COUNTDOWN b ->
                GameThread.defer
                <| fun () ->
                    match lobby with
                    | None -> Logging.Debug "Unexpected lobby packet: %A" packet
                    | Some lobby -> lobby.Countdown <- b
            | Downstream.GAME_START ->
                GameThread.defer
                <| fun () ->
                    match lobby with
                    | None -> Logging.Debug "Unexpected lobby packet: %A" packet
                    | Some lobby -> lobby.GameStart()
            | Downstream.GAME_END ->
                GameThread.defer
                <| fun () ->
                    match lobby with
                    | None -> Logging.Debug "Unexpected lobby packet: %A" packet
                    | Some lobby -> lobby.GameEnd()
            | Downstream.PLAY_DATA(username, timestamp, data) ->
                GameThread.defer
                <| fun () ->
                    match lobby with
                    | None -> Logging.Debug "Unexpected lobby packet: %A" packet
                    | Some lobby ->
                        match lobby.GetReplayInfo username with
                        | None -> Logging.Debug "Unexpected replay data for %s" username
                        | Some replay_info ->
                            use ms = new MemoryStream(data)
                            use br = new BinaryReader(ms)
                            (replay_info.Replay :?> OnlineReplay).ImportLiveBlock(Time.of_number timestamp, br)

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
            client.Connect()

    let login_with_token () =
        assert(GameThread.is_game_thread())
        client.Send(Upstream.LOGIN credentials.Token)

    let begin_login () =
        assert(GameThread.is_game_thread())
        client.Send(Upstream.LOGIN_WITH_DISCORD)

    let complete_registration (desired_username) =
        assert(GameThread.is_game_thread())
        client.Send(Upstream.COMPLETE_REGISTRATION_WITH_DISCORD desired_username)

    let logout () =
        assert(GameThread.is_game_thread())
        NetworkEvents.leave_lobby_ev.Trigger()
        lobby <- None

        if status = LoggedIn then
            client.Send Upstream.LOGOUT
            status <- Connected
            credentials.Token <- ""

    let disconnect () =
        if lobby.IsSome then
            GameThread.defer NetworkEvents.leave_lobby_ev.Trigger

        lobby <- None
        client.Disconnect()

    let init () =
        if target_ip.ToString() <> "0.0.0.0" then
            connect ()

    let deinit () =
        if status <> NotConnected then
            client.Disconnect()

        credentials.Save()

    let join_lobby id =
        assert(GameThread.is_game_thread())
        client.Send(Upstream.JOIN_LOBBY id)