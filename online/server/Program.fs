open System
open Percyqaz.Common
open Interlude.Web.Shared
open Interlude.Web.Server

let handle_packet(id: Guid, packet: Upstream) =
    async {
        match packet with

        | Upstream.DISCONNECT -> Server.kick(id, "User disconnect")

        | Upstream.VERSION v -> 
            if v = PROTOCOL_VERSION then UserState.handshake id
            else Server.kick(id, "Protocol version mismatch")
        | Upstream.LOGIN username ->
            UserState.login(id, username)

        | Upstream.GET_LOBBIES -> Lobby.list id
        | Upstream.JOIN_LOBBY lid -> Lobby.join (id, lid)
        | Upstream.CREATE_LOBBY name -> Lobby.create (id, name)

        | Upstream.INVITE_TO_LOBBY username -> Lobby.invite (id, username)
        | Upstream.LEAVE_LOBBY -> Lobby.leave id
        | Upstream.CHAT msg -> Lobby.chat (id, msg)
        | Upstream.READY_STATUS r -> Lobby.ready_up (id, r)

        | _ -> Server.kick(id, "Not yet implemented")

    } |> Async.Start

let handle_connect(id: Guid) = UserState.connect id
let handle_disconnect(id: Guid) = 
    UserState.disconnect id
    Lobby.leave id

Server.init { 
    Address = "0.0.0.0"; Port = 32767;
    Handle_Packet = handle_packet
    Handle_Connect = handle_connect
    Handle_Disconnect = handle_disconnect
}

Server.start()

Console.ReadLine() |> ignore

Server.stop()