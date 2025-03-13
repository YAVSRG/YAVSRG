open System
open System.IO
open System.Threading
open System.Security.Authentication
open System.Security.Cryptography.X509Certificates
open NetCoreServer
open Percyqaz.Common
open Interlude.Web.Shared
open Interlude.Web.Server
open Interlude.Web.Server.Online
open Interlude.Web.Server.API
open Interlude.Web.Server.Bot

let SOCKET_PORT = 32767
let HTTPS_PORT = 443

try
    Logging.Verbosity <- LoggingLevel.DEBUG
    Logging.Info "~~ Interlude.Web [%s] ~~" TAGLINE

    let api_cert =
        X509CertificateLoader.LoadPkcs12FromFile(Path.Combine("./secrets", SECRETS.ApiCert), SECRETS.ApiCertPassword)

    let socket_cert =
        X509CertificateLoader.LoadPkcs12FromFile(Path.Combine("./secrets", SECRETS.SocketCert), SECRETS.SocketCertPassword)

    Domain.Database.startup ()

    Server.init
        {
            Address = "0.0.0.0"
            Port = SOCKET_PORT
            SSLContext = SslContext(SslProtocols.Tls12, socket_cert, ClientCertificateRequired = false)
            Handle_Packet = Online.handle_packet
            Handle_Connect = Online.handle_connect
            Handle_Disconnect = Online.handle_disconnect
        }

    API.Server.init
        {
            Port = HTTPS_PORT
            SSLContext = SslContext(SslProtocols.Tls12, api_cert)
            Handle_Request = API.handle_request
        }

    Logging.Info "Launching game server on port %i ..." SOCKET_PORT
    Server.start ()
    Logging.Info "Launching api on port %i ..." HTTPS_PORT
    API.Server.start ()
    Logging.Info "Launching discord bot ..."
    Bot.start ()

#if DEBUG
    Console.ReadLine() |> ignore
#else
    Thread.Sleep Timeout.Infinite
#endif

with err ->
    Logging.Critical "%O" err
    Thread.Sleep 60000

Logging.Shutdown()