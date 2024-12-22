namespace Interlude.Web.Shared

open System.Net
open System.Net.Security
open System.Net.Sockets
open System.Security.Authentication
open NetCoreServer
open Percyqaz.Common

module private Client =

    type Config =
        {
            Address: IPAddress
            Port: int
            Handle_Packet: Downstream -> unit
            Handle_Connect: unit -> unit
            Handle_Disconnect: unit -> unit
        }

    let ssl_context () =
        SslContext(
            SslProtocols.Tls12,
            ClientCertificateRequired = false,
            CertificateValidationCallback = new RemoteCertificateValidationCallback(fun _ _ _ _ -> true)
        )

    type Session(config: Config) =
        inherit SslClient(ssl_context (), config.Address, config.Port)

        let buffer = ref Empty

        override this.OnHandshaked() =
            config.Handle_Connect()
            this.SendPacket(Upstream.VERSION PROTOCOL_VERSION)

        override this.OnDisconnected() = config.Handle_Disconnect()

        override this.OnReceived(data: byte array, offset: int64, size: int64) =
            try
                Buffer.handle (buffer, data, offset, size, Downstream.Read >> config.Handle_Packet)
            with err ->
                Logging.Error "Internal error processing socket data: %O" err
                this.Disconnect() |> ignore

        override this.OnError(error: SocketError) =
            Logging.Error "Socket error: %O" error

        member this.SendPacket(packet: Upstream) =
            let packet_with_header = Buffer.packet_bytes (packet.Write())
            this.SendAsync packet_with_header |> ignore

[<AbstractClass>]
type Client(address: IPAddress, port: int) as this =

    let session =
        new Client.Session(
            {
                Address = address
                Port = port
                Handle_Connect = this.OnConnected
                Handle_Disconnect = this.OnDisconnected
                Handle_Packet = this.OnPacketReceived
            }
        )

    member this.Connect() = session.ConnectAsync() |> ignore

    member this.Disconnect() = session.Disconnect() |> ignore

    member this.Send(packet: Upstream) =
        let packet_with_header = Buffer.packet_bytes (packet.Write())
        session.SendAsync packet_with_header |> ignore

    abstract member OnConnected: unit -> unit
    abstract member OnDisconnected: unit -> unit
    abstract member OnPacketReceived: Downstream -> unit