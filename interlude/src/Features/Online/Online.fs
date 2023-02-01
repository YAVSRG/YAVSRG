namespace Interlude.Features

open Percyqaz.Common
open Interlude.Web.Shared

module Online =

    let requested_username = "Percyqaz"

    let handle_packet(packet: Downstream) =
        match packet with
        | Downstream.DISCONNECT reason -> Logging.Info(sprintf "Disconnected from server: %s" reason)
        | Downstream.HANDSHAKE_SUCCESS -> Client.send(Upstream.LOGIN requested_username)
        | Downstream.LOGIN_SUCCESS username -> Logging.Info(sprintf "Successfully logged in as %s" username)
        | Downstream.CHAT _ -> ()

    let connect() =
        Client.init { Address = "127.0.0.1"; Port = 32767; Handle_Packet = handle_packet }
        Client.connect()