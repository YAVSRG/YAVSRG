namespace Interlude.Features.Online

open System
open System.Net
open System.IO
open System.Collections.Generic
open Percyqaz.Common
open Percyqaz.Json
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Data.Charts.Caching
open Prelude.Scoring
open Interlude.UI
open Interlude.Web.Shared

[<Json.AutoCodec>]
type Credentials =
    {
        DO_NOT_SHARE_THE_CONTENTS_OF_THIS_FILE_WITH_ANYONE_UNDER_ANY_CIRCUMSTANCES: string
        mutable Username: string
        mutable Host: string
    }
    static member Default =
        {
            DO_NOT_SHARE_THE_CONTENTS_OF_THIS_FILE_WITH_ANYONE_UNDER_ANY_CIRCUMSTANCES = "Doing so is equivalent to giving someone your account password"
            Username = ""
            Host = "online.yavsrg.net"
        }
    static member Location = Path.Combine(getDataPath "Data", "login.json") 
    static member Load() =
        if File.Exists Credentials.Location then
            File.SetAttributes(Credentials.Location, FileAttributes.Normal)
            Credentials.Location
            |> JSON.FromFile
            |> function Ok res -> res | Error e -> Logging.Error("Error loading login credentials, you will need to log in again.", e); Credentials.Default
        else Credentials.Default
    member this.Save() =
        JSON.ToFile (Credentials.Location, true) this
        File.SetAttributes(Credentials.Location, FileAttributes.Hidden)

module Network =

    type Status = 
        | NotConnected
        | Connecting
        | ConnectionFailed
        | Connected
        | LoggedIn

    let mutable status = NotConnected
    let mutable username = ""

    type LobbyPlayer =
        {
            mutable Status: LobbyPlayerStatus
            mutable Replay: OnlineReplayProvider
        }
        static member Create = 
            {
                Status = LobbyPlayerStatus.NotReady
                Replay = Unchecked.defaultof<_>
            }

    type Lobby =
        {
            mutable Settings: LobbySettings option
            Players: Dictionary<string, LobbyPlayer>
            mutable YouAreHost: bool
            mutable Ready: bool
            mutable Chart: LobbyChart option
            mutable Playing: bool
        }

    module Events =
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

        let game_start_ev = new Event<unit>()
        let game_start = game_start_ev.Publish

        let game_end_ev = new Event<unit>()
        let game_end = game_end_ev.Publish

    let mutable lobby : Lobby option = None

    let mutable lobby_list : LobbyInfo array = [||]

    let credentials = Credentials.Load()

    let target_ip = 
        try Dns.GetHostAddresses(credentials.Host).[0]
        with err -> Logging.Error("Failed to perform DNS lookup for " + credentials.Host, err); IPAddress.Parse("0.0.0.0")
    
    let client =
        { new Client(target_ip, 32767) with

            override this.OnConnected() = sync <| fun () -> 
                status <- Connected

            override this.OnDisconnected() = sync <| fun () -> 
                if lobby.IsSome then
                    lobby <- None
                    Events.leave_lobby_ev.Trigger()
                status <- if status = Connecting then ConnectionFailed else NotConnected

            override this.OnPacketReceived(packet: Downstream) =
                match packet with
                | Downstream.DISCONNECT reason ->
                    Logging.Info(sprintf "Disconnected from server: %s" reason)
                    Notifications.add(Localisation.localiseWith [reason] "notification.network.disconnected", NotificationType.Error)
                | Downstream.HANDSHAKE_SUCCESS -> if credentials.Username <> "" then this.Send(Upstream.LOGIN credentials.Username)
                | Downstream.LOGIN_SUCCESS name -> sync <| fun () -> 
                    Logging.Info(sprintf "Logged in as %s" name)
                    status <- LoggedIn
                    username <- name
                    Events.successful_login_ev.Trigger name

                | Downstream.LOBBY_LIST lobbies -> sync <| fun () ->
                    lobby_list <- lobbies
                    Events.receive_lobby_list_ev.Trigger()
                | Downstream.YOU_JOINED_LOBBY players -> sync <| fun () -> 
                    lobby <- Some {
                            Settings = None
                            Players =
                                let d = new Dictionary<string, LobbyPlayer>()
                                for p in players do d.Add(p, LobbyPlayer.Create)
                                d
                            YouAreHost = false
                            Ready = false
                            Chart = None
                            Playing = false
                        }
                    Events.join_lobby_ev.Trigger()
                | Downstream.INVITED_TO_LOBBY (by_user, lobby_id) -> sync <| fun () ->
                    Events.receive_invite_ev.Trigger(by_user, lobby_id)
                
                | Downstream.YOU_LEFT_LOBBY -> sync <| fun () ->
                    lobby <- None 
                    Events.leave_lobby_ev.Trigger()
                | Downstream.YOU_ARE_HOST b -> sync <| fun () ->
                    lobby.Value.YouAreHost <- b
                | Downstream.PLAYER_JOINED_LOBBY username -> sync <| fun () ->
                    lobby.Value.Players.Add(username, LobbyPlayer.Create)
                    Events.lobby_players_updated_ev.Trigger()
                | Downstream.PLAYER_LEFT_LOBBY username -> sync <| fun () ->
                    lobby.Value.Players.Remove(username) |> ignore
                    Events.lobby_players_updated_ev.Trigger()
                | Downstream.LOBBY_SETTINGS s -> sync <| fun () ->
                    lobby.Value.Settings <- Some s
                    Events.lobby_settings_updated_ev.Trigger()
                | Downstream.LOBBY_EVENT (kind, data) -> sync <| fun () ->
                    Events.lobby_event_ev.Trigger(kind, data)
                | Downstream.SYSTEM_MESSAGE msg -> 
                    Logging.Info(sprintf "[NETWORK] %s" msg)
                    sync <| fun () -> Events.system_message_ev.Trigger msg
                | Downstream.CHAT (sender, msg) -> 
                    Logging.Info(sprintf "%s: %s" sender msg)
                    sync <| fun () -> Events.chat_message_ev.Trigger(sender, msg)
                | Downstream.PLAYER_STATUS (username, status) -> sync <| fun () ->
                    lobby.Value.Players.[username].Status <- status
                    if status = LobbyPlayerStatus.Playing then
                        lobby.Value.Players.[username].Replay <- OnlineReplayProvider()
                    Events.lobby_players_updated_ev.Trigger()
                    Events.player_status_ev.Trigger(username, status)

                | Downstream.SELECT_CHART c -> sync <| fun () ->
                    lobby.Value.Chart <- Some c
                    for player in lobby.Value.Players.Values do player.Status <- LobbyPlayerStatus.NotReady
                    lobby.Value.Ready <- false
                    Events.change_chart_ev.Trigger()
                    Events.lobby_players_updated_ev.Trigger()
                | Downstream.GAME_START -> sync <| fun () ->
                    lobby.Value.Playing <- true
                    Events.game_start_ev.Trigger()
                | Downstream.GAME_END -> sync <| fun () ->
                    lobby.Value.Playing <- false
                    for player in lobby.Value.Players.Values do player.Status <- LobbyPlayerStatus.NotReady
                    lobby.Value.Ready <- false
                    Events.game_end_ev.Trigger()
                | Downstream.PLAY_DATA (username, data) -> sync <| fun () ->
                    use ms = new MemoryStream(data)
                    use br = new BinaryReader(ms)
                    lobby.Value.Players.[username].Replay.ImportLiveBlock br
        }

    let connect() =
        if status <> NotConnected && status <> ConnectionFailed then () else
        status <- Connecting

        Logging.Info(sprintf "Connecting to %s ..." credentials.Host)
        client.Connect()

    let login(name) = if status = Connected then client.Send(Upstream.LOGIN name)

    let logout() = 
        if lobby.IsSome then sync Events.leave_lobby_ev.Trigger
        lobby <- None
        if status = LoggedIn then
            client.Send(Upstream.LOGOUT)
            status <- Connected

    let disconnect() =
        if lobby.IsSome then sync Events.leave_lobby_ev.Trigger
        lobby <- None
        client.Disconnect()

    let shutdown() =
        if status <> NotConnected then client.Disconnect()
        credentials.Save()

module Lobby =

    open Network

    let chat msg = client.Send(Upstream.CHAT msg)
    let refresh_list() = client.Send(Upstream.GET_LOBBIES)
    let create name = client.Send(Upstream.CREATE_LOBBY name)
    let invite username = client.Send(Upstream.INVITE_TO_LOBBY username)
    let join id = client.Send(Upstream.JOIN_LOBBY id)
    let leave() = client.Send(Upstream.LEAVE_LOBBY)
    let set_ready flag = if not lobby.Value.Playing then client.Send(Upstream.READY_STATUS flag)
    let transfer_host username = client.Send(Upstream.TRANSFER_HOST username)
    let settings (settings: LobbySettings) = client.Send(Upstream.LOBBY_SETTINGS settings)
    let missing_chart() = client.Send(Upstream.MISSING_CHART)

    let start_playing() = client.Send Upstream.BEGIN_PLAYING
    let play_data data = client.Send (Upstream.PLAY_DATA data)
    let finish_playing() = client.Send (Upstream.FINISH_PLAYING false)
    let abandon_play() = client.Send (Upstream.FINISH_PLAYING true)

    let select_chart(cc: CachedChart, rate: float32) =
        if lobby.Value.YouAreHost then
            client.Send(Upstream.SELECT_CHART { Hash = cc.Hash; Artist = cc.Artist; Title = cc.Title; Creator = cc.Creator; Rate = rate })