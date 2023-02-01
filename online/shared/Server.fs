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
        }

    type private Session(server: TcpServer, config: Config) =
        inherit TcpSession(server)

        let buffer = ref Empty

        override this.OnConnected() =
            Logging.Info(sprintf ">> %O" this.Id)

        override this.OnDisconnected() =
            Logging.Info(sprintf "<< %O" this.Id)
        
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
            printfn "Error in TCP server: %O" error

    let mutable private server = Unchecked.defaultof<Listener>
    
    let init(config: Config) =
        server <- new Listener(config)

    let start() = 
        printfn "Starting server..."
        if server.Start() then printfn "Started server."

    let send(id: Guid, packet: Downstream) =
        let packet_with_header = Buffer.packet_bytes(packet.Write())
        server.FindSession(id).Send packet_with_header

    let stop() = 
        printfn "Shutting down server..."
        if server.Stop() then printfn "Server stopped."