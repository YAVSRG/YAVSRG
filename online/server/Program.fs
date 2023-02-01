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

        | Upstream.CHAT msg ->
            match! UserState.find_username id with
            | Some username -> printfn "%O: %s" username msg
            | None -> Server.kick(id, "Tried to send chat message before login")

        | Upstream.LOGIN username ->
            UserState.login(id, username)
    } |> Async.Start

let handle_connect(id: Guid) = UserState.connect id
let handle_disconnect(id: Guid) = UserState.disconnect id

Server.init { 
    Address = "0.0.0.0"; Port = 32767;
    Handle_Packet = handle_packet
    Handle_Connect = handle_connect
    Handle_Disconnect = handle_disconnect
}

Server.start()

Console.ReadLine() |> ignore

Server.stop()