namespace Interlude.Features.Multiplayer

open System.Net
open Percyqaz.Common
open Percyqaz.Flux.UI
open Interlude.UI
open System.Collections.Generic
open Interlude.Web.Shared

module Network =

    type Status = 
        | NotConnected
        | Connecting
        | ConnectionFailed
        | Connected
        | LoggedIn

    let mutable status = NotConnected
    let mutable requested_username = ""
    let mutable username = ""
    let mutable local = true
    let online_ip = try Dns.GetHostAddresses("online.yavsrg.net").[0] with err -> Logging.Error("Failed to perform DNS lookup for online.yavsrg.net"); IPAddress.Parse("127.0.0.1")

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
        }

    module Events =
        let receive_lobby_list_ev = new Event<unit>()
        let receive_lobby_list = receive_lobby_list_ev.Publish

        let join_lobby_ev = new Event<unit>()
        let join_lobby = join_lobby_ev.Publish

        let leave_lobby_ev = new Event<unit>()
        let leave_lobby = leave_lobby_ev.Publish
        
        let lobby_settings_updated_ev = new Event<unit>()
        let lobby_settings_updated = lobby_settings_updated_ev.Publish

    let mutable lobby : Lobby option = None

    let mutable lobby_list : LobbyInfo array = [||]
    
    let client =
        let target = 
            if local then IPAddress.Parse("127.0.0.1")
            else Dns.GetHostAddresses("online.yavsrg.net").[0]
        { new Client(target, 32767) with

            override this.OnConnected() = status <- Connected

            override this.OnDisconnected() = 
                lobby <- None
                status <- if status = Connecting then ConnectionFailed else NotConnected

            override this.OnPacketReceived(packet: Downstream) =
                printfn "%A" packet
                match packet with
                | Downstream.DISCONNECT reason -> Logging.Info(sprintf "Disconnected from server: %s" reason)

                | Downstream.HANDSHAKE_SUCCESS -> if requested_username <> "" then this.Send(Upstream.LOGIN requested_username)
                | Downstream.LOGIN_SUCCESS name -> 
                    Logging.Info(sprintf "Logged in as %s" name)
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
                        }
                    sync Events.join_lobby_ev.Trigger
                | Downstream.INVITED_TO_LOBBY (by_user, lobby_id) -> () // nyi
                
                | Downstream.YOU_LEFT_LOBBY -> lobby <- None; sync Events.leave_lobby_ev.Trigger
                | Downstream.YOU_ARE_HOST -> lobby.Value.YouAreHost <- true
                | Downstream.PLAYER_JOINED_LOBBY username -> lobby.Value.Players.Add(username, { IsReady = false; IsSpectating = false })
                | Downstream.PLAYER_LEFT_LOBBY username -> lobby.Value.Players.Remove(username) |> ignore
                | Downstream.SELECT_CHART _ -> () // nyi
                | Downstream.LOBBY_SETTINGS s -> lobby.Value.Settings <- Some s; sync Events.lobby_settings_updated_ev.Trigger
                | Downstream.SYSTEM_MESSAGE msg -> Logging.Info msg
                | Downstream.CHAT (sender, msg) -> Logging.Info(sprintf "<%s> %s" sender msg)
                | Downstream.READY_STATUS (username, ready) -> lobby.Value.Players.[username].IsReady <- ready

                | _ -> () // nyi
        }

    let connect() =
        if status <> NotConnected && status <> ConnectionFailed then () else
        status <- Connecting

        if local then Logging.Info "Connecting to local instance of server ..."
        else Logging.Info(sprintf "Connecting to online.yavsrg.net [%O] ..." online_ip)
        client.Connect()

    let login(name) =
        requested_username <- name
        if status = Connected then client.Send(Upstream.LOGIN name)

    let logout() = 
        if status = LoggedIn then
            client.Send(Upstream.LOGOUT)
            status <- Connected

    let disconnect() =
        client.Disconnect()

    let refresh_lobby_list() =
        client.Send(Upstream.GET_LOBBIES)

    let create_lobby name =
        client.Send(Upstream.CREATE_LOBBY name)

    let join_lobby id =
        client.Send(Upstream.JOIN_LOBBY id)
    
    let leave_lobby() =
        client.Send(Upstream.LEAVE_LOBBY)