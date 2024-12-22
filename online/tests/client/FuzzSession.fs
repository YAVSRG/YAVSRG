namespace Interlude.Web.Tests.Client

open System.Net.Security
open System.Net.Sockets
open System.Security.Authentication
open NetCoreServer
open Percyqaz.Common
open Interlude.Web.Shared

module FuzzSession =

    let ssl_context () =
        SslContext(
            SslProtocols.Tls12,
            ClientCertificateRequired = false,
            CertificateValidationCallback = new RemoteCertificateValidationCallback(fun _ _ _ _ -> true)
        )

type FuzzSession(name: string, handle_packet: Downstream -> unit) =
    inherit SslClient(FuzzSession.ssl_context (), System.Net.IPAddress.Parse("127.0.0.1"), 32767)

    let buffer = ref Empty

    override this.OnDisconnected() = Logging.Debug "Fuzz client (%s) disconnected" name

    override this.OnReceived(data: byte array, offset: int64, size: int64) =
        ()
        //try
        //    Buffer.handle (buffer, data, offset, size, Downstream.Read >> handle_packet)
        //with err ->
        //    Logging.Error "Internal error processing socket data: %O" err
        //    this.Disconnect() |> ignore

    override this.OnError(error: SocketError) =
        Logging.Error "Fuzz session (%s) socket error: %O" name error

    member this.SendPacket(packet: Upstream) =
        let packet_with_header = Buffer.packet_bytes (packet.Write())
        this.SendAsync packet_with_header |> ignore

    member this.SendRaw(bytes: byte array) =
        this.SendAsync bytes |> ignore