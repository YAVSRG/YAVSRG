open System
open System.IO
open System.Threading
open System.Security.Authentication
open System.Security.Cryptography.X509Certificates
open System.Reflection
open Percyqaz.Common
open Percyqaz.Json
open Interlude.Web.Shared
open Interlude.Web.Server.Online
open Interlude.Web.Server.API

[<Json.AutoCodec(false)>]
type Secrets = 
    {
        SocketCert: string
        SocketCertPassword: string
        ApiCert: string
        ApiCertPassword: string
        BotToken: string
    }
    static member Default = {
            SocketCert = "localhost.pfx"
            SocketCertPassword = "DEVELOPMENT"
            ApiCert = "localhost.pfx"
            ApiCertPassword = "DEVELOPMENT"
            BotToken = ""
        }

let SOCKET_PORT = 32767
let HTTPS_PORT = 443

let tagline = 
    let stream = Assembly.GetExecutingAssembly().GetManifestResourceStream("Interlude.Web.Server.Version.txt")
    use tr = new StreamReader(stream)
    tr.ReadToEnd()

try
    let secrets =
        if not (Directory.Exists "./secrets") then
            #if DEBUG
                failwith "Secrets folder not found! You are running the server as a non-docker instance, make sure you ran it with the script"
            #else
                failwith "Secrets folder not found! Did you mount it properly?"
            #endif
        match Prelude.Common.JSON.FromFile<Secrets>("./secrets/secrets.json") with
        | Ok o -> o
        | Error e ->
            failwithf "Error while reading secrets.json: %O" e

    let api_cert = new X509Certificate2(Path.Combine("./secrets", secrets.ApiCert), secrets.ApiCertPassword)
    let socket_cert = new X509Certificate2(Path.Combine("./secrets", secrets.SocketCert), secrets.SocketCertPassword)

    Logging.Info(sprintf "~~ Interlude.Web [%s] ~~" tagline)

    Server.init { 
        Address = "0.0.0.0"
        Port = SOCKET_PORT
        Handle_Packet = Online.handle_packet
        Handle_Connect = Online.handle_connect
        Handle_Disconnect = Online.handle_disconnect
    }
    
    API.init {
        Port = HTTPS_PORT
        SSLContext = NetCoreServer.SslContext(SslProtocols.Tls12, api_cert)
        Handle_Request = API.handle_request
    }

    Logging.Info(sprintf "Launching game server on port %i ..." SOCKET_PORT)
    Server.start()
    Logging.Info(sprintf "Launching api on port %i ..." HTTPS_PORT)
    API.start()
    
    #if DEBUG
    Console.ReadLine() |> ignore
    #else
    Thread.Sleep Timeout.Infinite
    #endif

with err ->
    Logging.Critical (err.ToString(), err)
    Thread.Sleep 60000

Logging.Shutdown()