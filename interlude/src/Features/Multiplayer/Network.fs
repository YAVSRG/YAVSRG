namespace Interlude.Features.Multiplayer

open System.Net
open System.IO
open System.Collections.Generic
open Percyqaz.Common
open Percyqaz.Json
open Percyqaz.Flux.UI
open Prelude.Common
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
            mutable IsReady: bool
            mutable IsSpectating: bool
        }

    type Lobby =
        {
            mutable Settings: LobbySettings option
            Players: Dictionary<string, LobbyPlayer>
            mutable YouAreHost: bool
            mutable Ready: bool
            mutable Chart: LobbyChart option
        }

    module Events =
        let successful_login_ev = new Event<string>()
        let successful_login = successful_login_ev.Publish

        let receive_lobby_list_ev = new Event<unit>()
        let receive_lobby_list = receive_lobby_list_ev.Publish

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
        
        let change_chart_ev = new Event<unit>()
        let change_chart = change_chart_ev.Publish

    let mutable lobby : Lobby option = None

    let mutable lobby_list : LobbyInfo array = [||]

    let credentials = Credentials.Load()

    let target_ip = 
        try Dns.GetHostAddresses(credentials.Host).[0]
        with err -> Logging.Error("Failed to perform DNS lookup for " + credentials.Host, err); IPAddress.Parse("0.0.0.0")
    
    let client =
        { new Client(target_ip, 32767) with

            override this.OnConnected() = status <- Connected

            override this.OnDisconnected() = 
                lobby <- None
                status <- if status = Connecting then ConnectionFailed else NotConnected

            override this.OnPacketReceived(packet: Downstream) =
                //printfn "%A" packet
                match packet with
                | Downstream.DISCONNECT reason -> 
                    Logging.Info(sprintf "Disconnected from server: %s" reason)
                    Notifications.add(Localisation.localiseWith [reason] "notification.network.disconnected", NotificationType.Error)

                | Downstream.HANDSHAKE_SUCCESS -> if credentials.Username <> "" then this.Send(Upstream.LOGIN credentials.Username)
                | Downstream.LOGIN_SUCCESS name -> 
                    Logging.Info(sprintf "Logged in as %s" name)
                    sync(fun () -> Events.successful_login_ev.Trigger name)
                    status <- LoggedIn
                    username <- name

                | Downstream.LOBBY_LIST lobbies -> lobby_list <- lobbies; sync Events.receive_lobby_list_ev.Trigger
                | Downstream.YOU_JOINED_LOBBY players ->
                    lobby <- Some {
                            Settings = None
                            Players =
                                let d = new Dictionary<string, LobbyPlayer>()
                                for p in players do d.Add(p, { IsReady = false; IsSpectating = false })
                                d
                            YouAreHost = false
                            Ready = false
                            Chart = None
                        }
                    sync Events.join_lobby_ev.Trigger
                | Downstream.INVITED_TO_LOBBY (by_user, lobby_id) -> () // nyi
                | Downstream.SYSTEM_MESSAGE msg -> 
                    Logging.Info(sprintf "[NETWORK] %s" msg)
                    sync(fun () -> Events.system_message_ev.Trigger msg)
                
                | Downstream.YOU_LEFT_LOBBY -> lobby <- None; sync Events.leave_lobby_ev.Trigger
                | Downstream.YOU_ARE_HOST -> 
                    lobby.Value.YouAreHost <- true
                    match Interlude.Features.Gameplay.Chart.cacheInfo with
                    | Some cc -> this.Send(Upstream.SELECT_CHART { Hash = cc.Hash; Artist = cc.Artist; Title = cc.Title; Rate = Interlude.Features.Gameplay.rate.Value })
                    | None -> ()
                | Downstream.PLAYER_JOINED_LOBBY username -> 
                    lobby.Value.Players.Add(username, { IsReady = false; IsSpectating = false })
                    sync(Events.lobby_players_updated_ev.Trigger)
                | Downstream.PLAYER_LEFT_LOBBY username -> 
                    lobby.Value.Players.Remove(username) |> ignore
                    sync Events.lobby_players_updated_ev.Trigger
                | Downstream.SELECT_CHART c -> 
                    lobby.Value.Chart <- Some c
                    for player in lobby.Value.Players.Values do player.IsReady <- false
                    sync Events.change_chart_ev.Trigger
                    sync Events.lobby_players_updated_ev.Trigger
                | Downstream.LOBBY_SETTINGS s -> lobby.Value.Settings <- Some s; sync Events.lobby_settings_updated_ev.Trigger
                | Downstream.LOBBY_EVENT (kind, data) -> sync(fun () -> Events.lobby_event_ev.Trigger(kind, data))
                | Downstream.CHAT (sender, msg) -> 
                    Logging.Info(sprintf "%s: %s" sender msg)
                    sync(fun () -> Events.chat_message_ev.Trigger(sender, msg))
                | Downstream.READY_STATUS (username, ready) -> 
                    lobby.Value.Players.[username].IsReady <- ready
                    sync Events.lobby_players_updated_ev.Trigger

                | _ -> () // nyi
        }

    let connect() =
        if status <> NotConnected && status <> ConnectionFailed then () else
        status <- Connecting

        Logging.Info(sprintf "Connecting to %s ..." credentials.Host)
        client.Connect()

    let login(name) = if status = Connected then client.Send(Upstream.LOGIN name)

    let logout() = 
        lobby <- None
        sync Events.leave_lobby_ev.Trigger
        if status = LoggedIn then
            client.Send(Upstream.LOGOUT)
            status <- Connected

    let disconnect() =
        lobby <- None
        sync Events.leave_lobby_ev.Trigger
        client.Disconnect()

    let send_chat_message(msg) = client.Send(Upstream.CHAT msg)

    let refresh_lobby_list() = client.Send(Upstream.GET_LOBBIES)

    let create_lobby name = client.Send(Upstream.CREATE_LOBBY name)

    let invite_to_lobby username = client.Send(Upstream.INVITE_TO_LOBBY username)

    let join_lobby id = client.Send(Upstream.JOIN_LOBBY id)
    
    let leave_lobby() = client.Send(Upstream.LEAVE_LOBBY)

    let ready_status flag = client.Send(Upstream.READY_STATUS flag)

    let shutdown() =
        if status <> NotConnected then client.Disconnect()
        credentials.Save()