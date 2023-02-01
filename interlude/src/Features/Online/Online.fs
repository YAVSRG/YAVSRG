namespace Interlude.Features

open Percyqaz.Common
open Interlude.Web.Shared

module Online =

    let handle_packet(packet: Downstream) =
        match packet with
        | Downstream.DISCONNECT reason -> Logging.Info(sprintf "Disconnected from server: %s" reason)
        | Downstream.CHAT _ -> ()
        | Downstream.LOGIN_SUCCESS username -> Logging.Info(sprintf "Successfully logged in as %s" username)

    let connect() =
        Client.init { Address = "127.0.0.1"; Port = 32767; Username = "Percyqaz"; Handle_Packet = handle_packet }
        Client.connect()