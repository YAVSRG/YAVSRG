namespace Interlude.Web.Shared

open System.Net
open System.Net.Sockets
open NetCoreServer
open Percyqaz.Common

module API =

    type Config =
        {
            Port: int
            SSLContext: SslContext
        }

    type private Session(server: HttpsServer, config: Config) =
        inherit HttpsSession(server)

        override this.OnReceivedRequest(request: HttpRequest) =
            Logging.Info(sprintf "%O %s" request.Method request.Url)

            match request.Method with
            | "GET" -> this.SendResponseAsync(this.Response.MakeGetResponse("{}", "application/json"))
            | _ -> this.SendResponseAsync(this.Response.MakeErrorResponse(404, "Not found"))
            |> ignore

        override this.OnReceivedRequestError(request: HttpRequest, error: string) =
            Logging.Error(sprintf "Error handling http request: %s" error)
        
        override this.OnError(error: SocketError) =
            Logging.Error(sprintf "Socket error in http session %O: %O" this.Id error)

    type private Listener(config: Config) =
        inherit HttpsServer(
            config.SSLContext,
            IPAddress.Any,
            config.Port)

        override this.CreateSession() = new Session(this, config)
        override this.OnError(error: SocketError) =
            Logging.Error(sprintf "Error in http server: %O" error)

    let mutable private server = Unchecked.defaultof<Listener>
    
    let init(config: Config) =
        server <- new Listener(config)

    let start() = 
        Logging.Info "Starting HTTP server..."
        if server.Start() then Logging.Info "Started HTTP server."

    let stop() = 
        Logging.Info "Stopping HTTP server..."
        if server.Stop() then Logging.Info "Stopped HTTP server."