namespace Interlude.Features.Multiplayer

open Percyqaz.Common
open System.Collections.Generic
open Interlude.Web.Shared

module Network =

    let mutable username = ""
    let mutable connected = false

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

    let mutable lobby : Lobby option = None

    let mutable lobby_list : LobbyInfo array = [||]

    let handle_connect() = connected <- true
    let handle_disconnect() = lobby <- None; connected <- false

    let handle_packet(packet: Downstream) =
        printfn "%A" packet
        match packet with
        | Downstream.DISCONNECT reason -> Logging.Info(sprintf "Disconnected from server: %s" reason)

        | Downstream.HANDSHAKE_SUCCESS -> Client.send(Upstream.LOGIN username)
        | Downstream.LOGIN_SUCCESS username -> 
            Logging.Info(sprintf "Successfully logged in as %s" username)
            Client.send(Upstream.CREATE_LOBBY "Interlude's First Lobby")

        | Downstream.LOBBY_LIST lobbies -> () // nyi
        | Downstream.YOU_JOINED_LOBBY players ->
            lobby <- Some {
                    Settings = None
                    Players =
                        let d = new Dictionary<string, LobbyPlayer>()
                        for p in players do d.Add(p, { IsReady = false; IsSpectating = false })
                        d
                    YouAreHost = false
                }
        | Downstream.INVITED_TO_LOBBY (by_user, lobby_id) -> () // nyi
        
        | Downstream.YOU_LEFT_LOBBY -> lobby <- None
        | Downstream.YOU_ARE_HOST -> lobby.Value.YouAreHost <- true
        | Downstream.PLAYER_JOINED_LOBBY username -> lobby.Value.Players.Add(username, { IsReady = false; IsSpectating = false })
        | Downstream.PLAYER_LEFT_LOBBY username -> lobby.Value.Players.Remove(username) |> ignore
        | Downstream.SELECT_CHART _ -> () // nyi
        | Downstream.LOBBY_SETTINGS s -> lobby.Value.Settings <- Some s
        | Downstream.SYSTEM_MESSAGE msg -> Logging.Info msg
        | Downstream.CHAT (sender, msg) -> Logging.Info(sprintf "<%s> %s" sender msg)
        | Downstream.READY_STATUS (username, ready) -> lobby.Value.Players.[username].IsReady <- ready

        | _ -> () // nyi

    let connect(name) =
        username <- name
        Client.init 
            { 
                Address = "127.0.0.1"
                Port = 32767
                Handle_Packet = handle_packet
                Handle_Connect = handle_connect
                Handle_Disconnect = handle_disconnect
            }
        Client.connect()