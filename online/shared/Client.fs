namespace Interlude.Web.Shared

open System.Net.Sockets
open NetCoreServer
open Percyqaz.Common

module Client = 

    type Config =
        {
            Address: string
            Port: int
            Handle_Packet: Downstream -> unit
            Handle_Connect: unit -> unit
            Handle_Disconnect: unit -> unit
        }

    type private Session(config: Config) =
        inherit TcpClient(config.Address, config.Port)

        let buffer = ref Empty

        override this.OnConnected() =
            Logging.Info "Connected to server!"
            this.SendPacket(Upstream.VERSION PROTOCOL_VERSION)

        override this.OnDisconnected() =
            Logging.Info "Disconnected from server."

        override this.OnReceived(data: byte array, offset: int64, size: int64) =
            try Buffer.handle(buffer, data, offset, size, Downstream.Read >> config.Handle_Packet)
            with err ->
                Logging.Error(sprintf "Internal error processing socket data: %O" err)
                this.SendPacket Upstream.DISCONNECT
                this.Disconnect() |> ignore

        override this.OnError(error: SocketError) =
            Logging.Error(sprintf "Socket error: %O" error)

        member this.SendPacket(packet: Upstream) =
            let packet_with_header = Buffer.packet_bytes(packet.Write())
            this.SendAsync packet_with_header |> ignore

    let mutable private session = Unchecked.defaultof<Session>

    let init(config: Config) =
        session <- new Session(config)

    let connect() =
        session.ConnectAsync() |> ignore

    let send(packet: Upstream) =
        let packet_with_header = Buffer.packet_bytes(packet.Write())
        session.SendAsync packet_with_header |> ignore