namespace Interlude.Web.Shared

open System
open System.Net.Sockets
open NetCoreServer
open Percyqaz.Common

module Server =

    type Config =
        {
            Address: string
            Port: int
            Handle_Packet: Guid * Upstream -> unit
            Handle_Connect: Guid -> unit
            Handle_Disconnect: Guid -> unit
        }

    type private Session(server: TcpServer, config: Config) =
        inherit TcpSession(server)

        let buffer = ref Empty

        override this.OnConnected() =
            config.Handle_Connect this.Id

        override this.OnDisconnected() =
            config.Handle_Disconnect this.Id
        
        override this.OnError(error: SocketError) =
            Logging.Error(sprintf "Socket error in session %O: %O" this.Id error)

        override this.OnReceived(data: byte array, offset, size) =
            try Buffer.handle(buffer, data, offset, size, Upstream.Read >> fun packet -> config.Handle_Packet(this.Id, packet))
            with err ->
                Logging.Error(sprintf "Internal error processing socket data: %O" err)
                this.ProtocolDisconnect "Internal error"

        member this.SendPacket(packet: Downstream) =
            let packet_with_header = Buffer.packet_bytes(packet.Write())
            this.Send packet_with_header

        member this.ProtocolDisconnect(reason: string) =
            this.SendPacket(Downstream.DISCONNECT reason) |> ignore
            this.Disconnect() |> ignore

    type private Listener(config: Config) =
        inherit TcpServer(config.Address, config.Port)

        override this.CreateSession() = new Session(this, config)
        override this.OnError(error: SocketError) =
            Logging.Error(sprintf "Error in TCP server: %O" error)

    let mutable private server = Unchecked.defaultof<Listener>
    
    let init(config: Config) =
        server <- new Listener(config)

    let start() = 
        Logging.Info "Starting server..."
        if server.Start() then Logging.Info "Started server."

    let send(id: Guid, packet: Downstream) =
        let packet_with_header = Buffer.packet_bytes(packet.Write())
        let session = server.FindSession(id)
        if not (isNull session) then
            if session.IsDisposed || session.IsSocketDisposed then Logging.Debug(sprintf "Can't send packet to %O, already disconnected" id)
            else session.Send packet_with_header |> ignore

    let kick(id: Guid, reason: string) =
        Logging.Info (sprintf "Kicking session %O: %s" id reason)
        send(id, Downstream.DISCONNECT reason)
        let session = server.FindSession(id)
        if not (isNull session) then session.Disconnect() |> ignore

    let stop() = 
        Logging.Info "Stopping server..."
        if server.Stop() then Logging.Info "Stopped server."