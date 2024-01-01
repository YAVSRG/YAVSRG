namespace Interlude.Features.Online

open System
open System.Net
open System.IO
open System.Collections.Generic
open Percyqaz.Common
open Percyqaz.Json
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay.Mods
open Prelude.Data.Charts.Caching
open Prelude.Gameplay
open Interlude.UI
open Interlude.Utils
open Interlude.Web.Shared

[<Json.AutoCodec(false)>]
type Credentials =
    {
        DO_NOT_SHARE_THE_CONTENTS_OF_THIS_FILE_WITH_ANYONE_UNDER_ANY_CIRCUMSTANCES: string
        mutable Username: string
        mutable Token: string
        mutable Host: string
        mutable Api: string
    }
    static member Default =
        {
            DO_NOT_SHARE_THE_CONTENTS_OF_THIS_FILE_WITH_ANYONE_UNDER_ANY_CIRCUMSTANCES =
                "Doing so is equivalent to giving someone your account password"
            Username = ""
            Token = ""
            Host = "online.yavsrg.net"
            Api = "api.yavsrg.net"
        }

    static member Location = Path.Combine(get_game_folder "Data", "login.json")

    static member Load() =
        if File.Exists Credentials.Location then
            File.SetAttributes(Credentials.Location, FileAttributes.Normal)

            Credentials.Location
            |> JSON.FromFile
            |> function
                | Ok res -> res
                | Error e ->
                    Logging.Error("Error loading login credentials, you will need to log in again.", e)
                    Credentials.Default
        else
            Credentials.Default

    member this.Save() =
        JSON.ToFile (Credentials.Location, true) this

module Network =

    type Status =
        | NotConnected
        | Connecting
        | ConnectionFailed
        | Connected
        | LoggedIn

    let mutable status = NotConnected

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
            mutable Settings: LobbySettings option
            Players: Dictionary<string, LobbyPlayer>
            mutable YouAreHost: bool
            mutable Spectate: bool
            mutable ReadyStatus: ReadyFlag
            mutable Chart: LobbyChart option
            mutable GameInProgress: bool
            mutable Countdown: bool
        }

    module Events =
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

    let mutable lobby: Lobby option = None

    let mutable lobby_list: LobbyInfo array = [||]

    let credentials = Credentials.Load()

    let target_ip =
        try
            Dns.GetHostAddresses(credentials.Host).[0]
        with err ->
            Logging.Error("Failed to perform DNS lookup for " + credentials.Host, err)
            IPAddress.Parse("0.0.0.0")

    let client =
        { new Client(target_ip, 32767) with

            override this.OnConnected() = sync <| fun () -> status <- Connected

            override this.OnDisconnected() =
                sync
                <| fun () ->
                    if lobby.IsSome then
                        lobby <- None
                        Events.leave_lobby_ev.Trigger()

                    status <-
                        if status = Connecting then
                            ConnectionFailed
                        else
                            NotConnected

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
                    Events.waiting_registration_ev.Trigger discord_tag
                | Downstream.REGISTRATION_FAILED reason ->
                    Logging.Info(sprintf "Registration failed: %s" reason)
                    Notifications.error (%"notification.network.registrationfailed", reason)
                    Events.registration_failed_ev.Trigger reason
                | Downstream.AUTH_TOKEN token ->
                    credentials.Token <- token
                    this.Send(Upstream.LOGIN credentials.Token)
                | Downstream.LOGIN_SUCCESS name ->
                    sync
                    <| fun () ->
                        Logging.Info(sprintf "Logged in as %s" name)
                        credentials.Username <- name
                        API.Client.authenticate credentials.Token
                        status <- LoggedIn

                        if Screen.current_type <> Screen.Type.SplashScreen then
                            Notifications.system_feedback (Icons.GLOBE, [ name ] %> "notification.network.login", "")

                        Events.successful_login_ev.Trigger name
                | Downstream.LOGIN_FAILED reason ->
                    credentials.Token <- ""
                    Logging.Info(sprintf "Login failed: %s" reason)

                    if Screen.current_type <> Screen.Type.SplashScreen then
                        Notifications.error (%"notification.network.loginfailed", reason)

                    Events.login_failed_ev.Trigger reason

                | Downstream.LOBBY_LIST lobbies ->
                    sync
                    <| fun () ->
                        lobby_list <- lobbies
                        Events.receive_lobby_list_ev.Trigger()
                | Downstream.YOU_JOINED_LOBBY players ->
                    sync
                    <| fun () ->
                        lobby <-
                            Some
                                {
                                    Settings = None
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

                        Events.join_lobby_ev.Trigger()
                | Downstream.INVITED_TO_LOBBY(by_user, lobby_id) ->
                    sync <| fun () -> Events.receive_invite_ev.Trigger(by_user, lobby_id)

                | Downstream.YOU_LEFT_LOBBY ->
                    sync
                    <| fun () ->
                        lobby <- None
                        Events.leave_lobby_ev.Trigger()
                | Downstream.YOU_ARE_HOST b -> sync <| fun () -> lobby.Value.YouAreHost <- b
                | Downstream.PLAYER_JOINED_LOBBY(username, color) ->
                    sync
                    <| fun () ->
                        lobby.Value.Players.Add(username, LobbyPlayer.Create color)
                        Events.lobby_players_updated_ev.Trigger()
                | Downstream.PLAYER_LEFT_LOBBY username ->
                    sync
                    <| fun () ->
                        lobby.Value.Players.Remove(username) |> ignore
                        Events.lobby_players_updated_ev.Trigger()
                | Downstream.SELECT_CHART c ->
                    sync
                    <| fun () ->
                        lobby.Value.Chart <- Some c

                        for player in lobby.Value.Players.Values do
                            player.Status <- LobbyPlayerStatus.NotReady

                        lobby.Value.ReadyStatus <- ReadyFlag.NotReady
                        Events.change_chart_ev.Trigger()
                        Events.lobby_players_updated_ev.Trigger()
                | Downstream.LOBBY_SETTINGS s ->
                    sync
                    <| fun () ->
                        lobby.Value.Settings <- Some s
                        Events.lobby_settings_updated_ev.Trigger()
                | Downstream.LOBBY_EVENT(kind, data) -> sync <| fun () -> Events.lobby_event_ev.Trigger(kind, data)
                | Downstream.SYSTEM_MESSAGE msg ->
                    Logging.Info(sprintf "[NETWORK] %s" msg)
                    sync <| fun () -> Events.system_message_ev.Trigger msg
                | Downstream.CHAT(sender, msg) ->
                    Logging.Info(sprintf "%s: %s" sender msg)
                    sync <| fun () -> Events.chat_message_ev.Trigger(sender, msg)
                | Downstream.PLAYER_STATUS(username, status) ->
                    sync
                    <| fun () ->
                        lobby.Value.Players.[username].Status <- status

                        if status = LobbyPlayerStatus.Playing then
                            lobby.Value.Players.[username].Replay <- OnlineReplayProvider()

                        Events.lobby_players_updated_ev.Trigger()
                        Events.player_status_ev.Trigger(username, status)
                | Downstream.COUNTDOWN(reason, seconds) ->
                    sync <| fun () -> Events.countdown_ev.Trigger(reason, seconds)

                | Downstream.GAME_COUNTDOWN b -> sync <| fun () -> lobby.Value.Countdown <- b
                | Downstream.GAME_START ->
                    sync
                    <| fun () ->
                        lobby.Value.Countdown <- false
                        lobby.Value.GameInProgress <- true
                        Events.game_start_ev.Trigger()
                | Downstream.GAME_END ->
                    sync
                    <| fun () ->
                        lobby.Value.GameInProgress <- false
                        Events.game_end_ev.Trigger()

                        for player in lobby.Value.Players.Values do
                            player.Status <- LobbyPlayerStatus.NotReady

                        lobby.Value.ReadyStatus <- ReadyFlag.NotReady
                | Downstream.PLAY_DATA(username, data) ->
                    sync
                    <| fun () ->
                        use ms = new MemoryStream(data)
                        use br = new BinaryReader(ms)
                        lobby.Value.Players.[username].Replay.ImportLiveBlock br
        }

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
        Events.leave_lobby_ev.Trigger()
        lobby <- None

        if status = LoggedIn then
            client.Send Upstream.LOGOUT
            status <- Connected
            credentials.Token <- ""

    let disconnect () =
        if lobby.IsSome then
            sync Events.leave_lobby_ev.Trigger

        lobby <- None
        client.Disconnect()

    let shutdown () =
        if status <> NotConnected then
            client.Disconnect()

        credentials.Save()

module Lobby =

    open Network

    let chat msg = client.Send(Upstream.CHAT msg)
    let refresh_list () = client.Send(Upstream.GET_LOBBIES)
    let create name = client.Send(Upstream.CREATE_LOBBY name)

    let invite username =
        client.Send(Upstream.INVITE_TO_LOBBY username)

    let join id = client.Send(Upstream.JOIN_LOBBY id)
    let leave () = client.Send(Upstream.LEAVE_LOBBY)

    let set_ready flag =
        if not lobby.Value.GameInProgress then
            client.Send(Upstream.READY_STATUS flag)

    let transfer_host username =
        client.Send(Upstream.TRANSFER_HOST username)

    let settings (settings: LobbySettings) =
        client.Send(Upstream.LOBBY_SETTINGS settings)

    let missing_chart () = client.Send(Upstream.MISSING_CHART)

    let start_round () = client.Send(Upstream.START_GAME)
    let cancel_round () = client.Send(Upstream.CANCEL_GAME)

    let start_playing () = client.Send(Upstream.BEGIN_PLAYING)
    let start_spectating () = client.Send(Upstream.BEGIN_SPECTATING)
    let play_data data = client.Send(Upstream.PLAY_DATA data)

    let finish_playing () =
        client.Send(Upstream.FINISH_PLAYING false)

    let abandon_play () =
        client.Send(Upstream.FINISH_PLAYING true)

    let select_chart (cc: CachedChart, rate: float32, mods: ModState) =
        if lobby.Value.YouAreHost then
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
