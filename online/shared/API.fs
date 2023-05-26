namespace Interlude.Web.Shared

open System
open System.Web
open System.Net
open System.Net.Sockets
open NetCoreServer
open Percyqaz.Common

module API =

    type HttpMethod =
        | GET
        | POST

    type Config =
        {
            Port: int
            SSLContext: SslContext
            Handle_Request: HttpMethod * string * string * Map<string, string array> * Map<string, string> * HttpResponse -> Async<unit>
        }

    let base_uri = Uri("https://localhost/")

    type private Session(server: HttpsServer, config: Config) =
        inherit HttpsSession(server)

        override this.OnReceivedRequest(request: HttpRequest) =
            let uri = new Uri(base_uri, request.Url)
            let query_params =
                let from_uri = HttpUtility.ParseQueryString uri.Query
                seq { for p in from_uri do yield (p, from_uri.GetValues p) } |> Map.ofSeq
            let headers =
                seq { for i = 0 to int request.Headers - 1 do let struct (key, value) = request.Header i in (key, value) } |> Map.ofSeq

            try
                match request.Method with
                | "GET" ->
                    config.Handle_Request (GET, uri.AbsolutePath, request.Body, query_params, headers, this.Response) |> Async.RunSynchronously
                    this.SendResponseAsync this.Response
                | "POST" -> 
                    config.Handle_Request (POST, uri.AbsolutePath, request.Body, query_params, headers, this.Response) |> Async.RunSynchronously
                    this.SendResponseAsync this.Response
                | _ -> this.SendResponseAsync(this.Response.MakeErrorResponse(404, "Not found"))
                |> ignore
            with e ->
                Logging.Critical(sprintf "Error handling HTTP request %O" request, e)

        override this.OnReceivedRequestError(request: HttpRequest, error: string) =
            Logging.Error(sprintf "Error handling HTTP request: %s" error)
        
        override this.OnError(error: SocketError) =
            Logging.Error(sprintf "Socket error in HTTP session %O: %O" this.Id error)

    type private Listener(config: Config) =
        inherit HttpsServer(
            config.SSLContext,
            IPAddress.Any,
            config.Port)

        override this.CreateSession() = new Session(this, config)
        override this.OnError(error: SocketError) =
            Logging.Error(sprintf "Error in HTTP server: %O" error)

    let mutable private server = Unchecked.defaultof<Listener>
    
    let init(config: Config) =
        server <- new Listener(config)

    let start() = 
        if server.Start() then Logging.Info "HTTP server is listening!"

    let stop() = 
        if server.Stop() then Logging.Info "Stopped HTTP server."