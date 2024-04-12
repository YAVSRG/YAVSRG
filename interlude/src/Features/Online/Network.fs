namespace Interlude.Features.Online

open System
open System.Net
open System.IO
open System.Collections.Generic
open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay.Mods
open Prelude.Data.Library.Caching
open Prelude.Gameplay
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

    let receive_lobby_list_ev = new Event<unit>()
    let receive_lobby_list = receive_lobby_list_ev.Publish

    let receive_invite_ev = new Event<string * Guid>()
    let receive_invite = receive_invite_ev.Publish

    let join_lobby_ev = new Event<unit>()
    let join_lobby = join_lobby_ev.Publish

    let chat_message_ev = new Event<string * string>()
    let chat_message = chat_message_ev.Publish

    let system_message_ev = new Event<string>()
    let system_message = system_message_ev.Publish

    let leave_lobby_ev = new Event<unit>()
    let leave_lobby = leave_lobby_ev.Publish

    let lobby_settings_updated_ev = new Event<unit>()
    let lobby_settings_updated = lobby_settings_updated_ev.Publish

    let lobby_event_ev = new Event<LobbyEvent * string>()
    let lobby_event = lobby_event_ev.Publish

    let lobby_players_updated_ev = new Event<unit>()
    let lobby_players_updated = lobby_players_updated_ev.Publish

    let player_status_ev = new Event<string * LobbyPlayerStatus>()
    let player_status = player_status_ev.Publish

    let change_chart_ev = new Event<unit>()
    let change_chart = change_chart_ev.Publish

    let countdown_ev = new Event<string * int>()
    let countdown = countdown_ev.Publish

    let game_start_ev = new Event<unit>()
    let game_start = game_start_ev.Publish

    let game_end_ev = new Event<unit>()
    let game_end = game_end_ev.Publish

module Network =

    type Status =
        | NotConnected
        | Connecting
        | ConnectionFailed
        | Connected
        | LoggedIn

    let mutable status = NotConnected

    type LobbyInvite =
        {
            InvitedBy: string
            LobbyId: Guid
        }

    type LobbyPlayer =
        {
            Color: Color
            mutable Status: LobbyPlayerStatus
            mutable Replay: OnlineReplayProvider
        }
        static member Create color =
            {
                Color = Color.FromArgb color
                Status = LobbyPlayerStatus.NotReady
                Replay = Unchecked.defaultof<_>
            }

    type Lobby =
        {
            Client: Client
            mutable Settings: LobbySettings
            Players: Dictionary<string, LobbyPlayer>
            mutable YouAreHost: bool
            mutable Spectate: bool
            mutable ReadyStatus: ReadyFlag
            mutable Chart: LobbyChart option
            mutable GameInProgress: bool
            mutable Countdown: bool
        }
        member this.SendMessage(msg: string) = this.Client.Send(Upstream.CHAT msg)
        member this.InvitePlayer(username: string) = this.Client.Send(Upstream.INVITE_TO_LOBBY username)
        member this.Leave () = this.Client.Send(Upstream.LEAVE_LOBBY)

        member this.TransferHost(username: string) = this.Client.Send(Upstream.TRANSFER_HOST username)
        member this.ChangeSettings (settings: LobbySettings) = this.Client.Send(Upstream.LOBBY_SETTINGS settings)

        member this.ReportMissingChart() = this.Client.Send(Upstream.MISSING_CHART)
        member this.SetReadyStatus(flag: ReadyFlag) =
            if not this.GameInProgress then
                this.ReadyStatus <- flag
                this.Client.Send(Upstream.READY_STATUS flag)

        member this.StartRound() = this.Client.Send(Upstream.START_GAME)
        member this.CancelRound() = this.Client.Send(Upstream.CANCEL_GAME)
        member this.StartPlaying() = this.Client.Send(Upstream.BEGIN_PLAYING)
        member this.StartSpectating() = this.Client.Send(Upstream.BEGIN_SPECTATING)
        member this.SendReplayData data = this.Client.Send(Upstream.PLAY_DATA data)
        member this.FinishPlaying() = this.Client.Send(Upstream.FINISH_PLAYING false)
        member this.AbandonPlaying() = this.Client.Send(Upstream.FINISH_PLAYING true)

        member this.SelectChart (cc: CachedChart, rate: float32, mods: ModState) =
            if this.YouAreHost then
                this.Client.Send(
                    Upstream.SELECT_CHART
                        {
                            Hash = cc.Hash
                            Artist = cc.Artist
                            Title = cc.Title
                            Creator = cc.Creator
                            Rate = rate
                            Mods = Map.toArray mods
                        }
                )

    let mutable lobby_list: LobbyInfo array = [||]
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
                    lobby_list <- lobbies
                    NetworkEvents.receive_lobby_list_ev.Trigger()
            | Downstream.YOU_JOINED_LOBBY players ->
                sync
                <| fun () ->
                    lobby <-
                        Some
                            {
                                Client = this
                                Settings = LobbySettings.Default
                                Players =
                                    let d = new Dictionary<string, LobbyPlayer>()

                                    for (username, color) in players do
                                        d.Add(username, LobbyPlayer.Create color)

                                    d
                                YouAreHost = false
                                Spectate = false
                                ReadyStatus = ReadyFlag.NotReady
                                Chart = None
                                GameInProgress = false
                                Countdown = false
                            }

                    NetworkEvents.join_lobby_ev.Trigger()
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
            | Downstream.YOU_ARE_HOST b -> sync <| fun () -> lobby.Value.YouAreHost <- b
            | Downstream.PLAYER_JOINED_LOBBY(username, color) ->
                sync
                <| fun () ->
                    lobby.Value.Players.Add(username, LobbyPlayer.Create color)
                    NetworkEvents.lobby_players_updated_ev.Trigger()
            | Downstream.PLAYER_LEFT_LOBBY username ->
                sync
                <| fun () ->
                    lobby.Value.Players.Remove(username) |> ignore
                    NetworkEvents.lobby_players_updated_ev.Trigger()
            | Downstream.SELECT_CHART c ->
                sync
                <| fun () ->
                    lobby.Value.Chart <- Some c

                    for player in lobby.Value.Players.Values do
                        player.Status <- LobbyPlayerStatus.NotReady

                    lobby.Value.ReadyStatus <- ReadyFlag.NotReady
                    NetworkEvents.change_chart_ev.Trigger()
                    NetworkEvents.lobby_players_updated_ev.Trigger()
            | Downstream.LOBBY_SETTINGS s ->
                sync
                <| fun () ->
                    lobby.Value.Settings <- s
                    NetworkEvents.lobby_settings_updated_ev.Trigger()
            | Downstream.LOBBY_EVENT(kind, data) -> sync <| fun () -> NetworkEvents.lobby_event_ev.Trigger(kind, data)
            | Downstream.SYSTEM_MESSAGE msg ->
                Logging.Info(sprintf "[NETWORK] %s" msg)
                sync <| fun () -> NetworkEvents.system_message_ev.Trigger msg
            | Downstream.CHAT(sender, msg) ->
                Logging.Info(sprintf "%s: %s" sender msg)
                sync <| fun () -> NetworkEvents.chat_message_ev.Trigger(sender, msg)
            | Downstream.PLAYER_STATUS(username, status) ->
                sync
                <| fun () ->
                    lobby.Value.Players.[username].Status <- status

                    if status = LobbyPlayerStatus.Playing then
                        lobby.Value.Players.[username].Replay <- OnlineReplayProvider()

                    NetworkEvents.lobby_players_updated_ev.Trigger()
                    NetworkEvents.player_status_ev.Trigger(username, status)
            | Downstream.COUNTDOWN(reason, seconds) ->
                sync <| fun () -> NetworkEvents.countdown_ev.Trigger(reason, seconds)

            | Downstream.GAME_COUNTDOWN b -> sync <| fun () -> lobby.Value.Countdown <- b
            | Downstream.GAME_START ->
                sync
                <| fun () ->
                    lobby.Value.Countdown <- false
                    lobby.Value.GameInProgress <- true
                    NetworkEvents.game_start_ev.Trigger()
            | Downstream.GAME_END ->
                sync
                <| fun () ->
                    lobby.Value.GameInProgress <- false
                    NetworkEvents.game_end_ev.Trigger()

                    for player in lobby.Value.Players.Values do
                        player.Status <- LobbyPlayerStatus.NotReady

                    lobby.Value.ReadyStatus <- ReadyFlag.NotReady
            | Downstream.PLAY_DATA(username, data) ->
                sync
                <| fun () ->
                    use ms = new MemoryStream(data)
                    use br = new BinaryReader(ms)
                    lobby.Value.Players.[username].Replay.ImportLiveBlock br

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

module Lobby =

    // todo: members of Lobby object
    open Network
    let refresh_list () = client.Send(Upstream.GET_LOBBIES)
    let create name = client.Send(Upstream.CREATE_LOBBY name)
    let join id = client.Send(Upstream.JOIN_LOBBY id)