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
open Prelude.Data
open Interlude.UI
open Interlude.Web.Shared

type LobbyInvite =
    {
        InvitedBy: string
        LobbyId: Guid
    }

type LobbyPlayer =
    {
        Color: Color
        mutable Status: LobbyPlayerStatus
    }
    static member Create color =
        {
            Color = Color.FromArgb color
            Status = LobbyPlayerStatus.NotReady
        }

type LobbyPlayerReplayInfo =
    {
        Replay: OnlineReplayProvider
        ScoreMetric: IScoreMetric
        GetScoreInfo: unit -> ScoreInfo
    }

type Lobby(client: Client, players: (string * int32) array) =

    let players =
        let d = new Dictionary<string, LobbyPlayer>()
        for (username, color) in players do
            d.Add(username, LobbyPlayer.Create color)
        d

    let replays =
        new Dictionary<string, LobbyPlayerReplayInfo>()

    let chat_message_ev = new Event<string * string>()
    let system_message_ev = new Event<string>()
    let lobby_settings_updated_ev = new Event<LobbySettings>() // pass settings
    let lobby_event_ev = new Event<LobbyEvent * string>()
    let lobby_players_updated_ev = new Event<unit>()
    let player_status_ev = new Event<string * LobbyPlayerStatus>()
    let change_chart_ev = new Event<unit>() // pass lobby chart
    let countdown_ev = new Event<string * int>()
    let game_start_ev = new Event<unit>()
    let game_end_ev = new Event<unit>()

    member this.OnChatMessage = chat_message_ev.Publish
    member this.OnSystemMessage = system_message_ev.Publish
    member this.OnSettingsUpdated = lobby_settings_updated_ev.Publish
    member this.OnLobbyEvent = lobby_event_ev.Publish
    member this.OnPlayersUpdated = lobby_players_updated_ev.Publish
    member this.OnPlayerStatusChanged = player_status_ev.Publish
    member this.OnChartChanged = change_chart_ev.Publish
    member this.OnCountdown = countdown_ev.Publish
    member this.OnGameStart = game_start_ev.Publish
    member this.OnGameEnd = game_end_ev.Publish

    member this.PlayerJoined(username: string, color: int32) =
        this.Players.[username] <- LobbyPlayer.Create color
        lobby_players_updated_ev.Trigger()
    
    member this.PlayerLeft(username: string) =
        this.Players.Remove(username) |> ignore
        lobby_players_updated_ev.Trigger()

    member this.ChartSelected(lc: LobbyChart) =
        this.Chart <- Some lc

        for player in this.Players.Values do
            player.Status <- LobbyPlayerStatus.NotReady

        this.ReadyStatus <- ReadyFlag.NotReady
        change_chart_ev.Trigger()
        lobby_players_updated_ev.Trigger()

    member this.UpdateSettings(settings: LobbySettings) =
        this.Settings <- settings
        lobby_settings_updated_ev.Trigger settings

    member this.LobbyEvent(kind: LobbyEvent, player: string) =
        lobby_event_ev.Trigger(kind, player)

    member this.SystemMessage(msg: string) =
        system_message_ev.Trigger msg

    member this.ChatMessage(sender: string, msg: string) =
        chat_message_ev.Trigger(sender, msg)

    member this.PlayerStatus(username: string, status: LobbyPlayerStatus) =
        this.Players.[username].Status <- status

        if status = LobbyPlayerStatus.Playing then
            this.Players.[username].Replay <- OnlineReplayProvider()

        lobby_players_updated_ev.Trigger()
        player_status_ev.Trigger(username, status)

    member this.StartCountdown(reason: string, seconds: int) =
        countdown_ev.Trigger(reason, seconds)

    member this.GameStart() =
        this.Countdown <- false
        this.GameInProgress <- true
        game_start_ev.Trigger()

    member this.GameEnd() =
        this.GameInProgress <- false
        for player in this.Players.Values do
            player.Status <- LobbyPlayerStatus.NotReady
        this.ReadyStatus <- ReadyFlag.NotReady
        game_end_ev.Trigger()

    member val Settings: LobbySettings = LobbySettings.Default with get, set
    member val Players: Dictionary<string, LobbyPlayer> = players with get
    member val YouAreHost: bool = false with get, set
    member val Spectate: bool = false with get, set
    member val ReadyStatus: ReadyFlag = ReadyFlag.NotReady with get, set
    member val Chart: LobbyChart option  = None with get, set
    member val GameInProgress: bool = false with get, set
    member val Countdown: bool = false with get, set

    member this.SendMessage(msg: string) = client.Send(Upstream.CHAT msg)
    member this.InvitePlayer(username: string) = client.Send(Upstream.INVITE_TO_LOBBY username)
    member this.Leave () = client.Send(Upstream.LEAVE_LOBBY)

    member this.TransferHost(username: string) = client.Send(Upstream.TRANSFER_HOST username)
    member this.ChangeSettings (settings: LobbySettings) = client.Send(Upstream.LOBBY_SETTINGS settings)

    member this.ReportMissingChart() = client.Send(Upstream.MISSING_CHART)
    member this.SetReadyStatus(flag: ReadyFlag) =
        if not this.GameInProgress then
            this.ReadyStatus <- flag
            client.Send(Upstream.READY_STATUS flag)

    member this.StartRound() = client.Send(Upstream.START_GAME)
    member this.CancelRound() = client.Send(Upstream.CANCEL_GAME)
    member this.StartPlaying() = client.Send(Upstream.BEGIN_PLAYING)
    member this.StartSpectating() = client.Send(Upstream.BEGIN_SPECTATING)
    member this.SendReplayData data = client.Send(Upstream.PLAY_DATA data)
    member this.FinishPlaying() = client.Send(Upstream.FINISH_PLAYING false)
    member this.AbandonPlaying() = client.Send(Upstream.FINISH_PLAYING true)

    member this.SelectChart (cc: CachedChart, rate: float32, mods: ModState) =
        if this.YouAreHost then
            client.Send(
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
                        use ms = new MemoryStream(data)
                        use br = new BinaryReader(ms)
                        lobby.Players.[username].Replay.ImportLiveBlock br

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