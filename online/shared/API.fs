namespace Interlude.Web.Shared

open System
open System.Web
open System.Net
open System.Net.Sockets
open NetCoreServer
open Percyqaz.Common
open Prelude

module API =

    let escape = Uri.EscapeDataString

    type HttpMethod =
        | GET
        | POST
        | DELETE

    type Config =
        {
            Port: int
            SSLContext: SslContext
            Handle_Request:
                HttpMethod * string * string * Map<string, string array> * Map<string, string> * HttpResponse
                    -> Async<unit>
        }

    let base_uri = Uri("https://localhost/")

    type private Session(server: HttpsServer, config: Config) =
        inherit HttpsSession(server)

        override this.OnReceivedRequest(request: HttpRequest) =
            let uri = new Uri(base_uri, request.Url)

            let query_params =
                let from_uri = HttpUtility.ParseQueryString uri.Query

                seq {
                    for p in from_uri do
                        yield (p, from_uri.GetValues p)
                }
                |> Map.ofSeq

            let headers =
                seq {
                    for i = 0 to int request.Headers - 1 do
                        let struct (key, value) = request.Header i in (key, value)
                }
                |> Map.ofSeq

            try
                match request.Method with
                | "GET" ->
                    config.Handle_Request(GET, uri.AbsolutePath, request.Body, query_params, headers, this.Response)
                    |> Async.RunSynchronously
                | "DELETE" ->
                    config.Handle_Request(DELETE, uri.AbsolutePath, request.Body, query_params, headers, this.Response)
                    |> Async.RunSynchronously
                | "POST" ->
                    config.Handle_Request(POST, uri.AbsolutePath, request.Body, query_params, headers, this.Response)
                    |> Async.RunSynchronously
                | _ -> this.Response.MakeErrorResponse(404, "Not found") |> ignore

                this.SendResponseAsync this.Response |> ignore
            with e ->
                Logging.Critical(sprintf "Error handling HTTP request %O" request, e)

        override this.OnReceivedRequestError(request: HttpRequest, error: string) =
            Logging.Error(sprintf "Error handling HTTP request: %s" error)

        override this.OnError(error: SocketError) =
            if error <> SocketError.NotConnected then
                Logging.Error(sprintf "Socket error in HTTP session %O: %O" this.Id error)

    type private Listener(config: Config) =
        inherit HttpsServer(config.SSLContext, IPAddress.Any, config.Port)

        override this.CreateSession() = new Session(this, config)

        override this.OnError(error: SocketError) =
            Logging.Error(sprintf "Error in HTTP server: %O" error)

    module Server =

        let mutable private server = Unchecked.defaultof<Listener>

        let init (config: Config) = server <- new Listener(config)

        let start () =
            if server.Start() then
                Logging.Info "HTTP server is listening!"

        let stop () =
            if server.Stop() then
                Logging.Info "Stopped HTTP server."

    module Client =

        let private client = new Http.HttpClient()

        let init (base_address: string) =
            client.BaseAddress <- new Uri(base_address)
            client.Timeout <- TimeSpan.FromSeconds(5.0)

        let authenticate (token: string) =
            client.DefaultRequestHeaders.Authorization <- new Http.Headers.AuthenticationHeaderValue("Bearer", token)

        let private queue =
            { new Async.Service<Http.HttpClient -> Async<unit>, unit>() with
                override this.Handle(action) = async { do! action client }
            }

        let internal get<'T> (route: string, callback: 'T option -> unit) =
            queue.Request(
                fun client ->
                    async {
                        try
                            let! response = client.GetAsync(route) |> Async.AwaitTask

                            if response.IsSuccessStatusCode then
                                match response.Content.ReadAsStream() |> fun s -> JSON.FromStream(route, s) with
                                | Ok res -> callback (Some res)
                                | Error err ->
                                    Logging.Error(sprintf "Error getting %s: %s" route err.Message)
                                    callback None
                            else
                                callback None
                        with
                        | :? Http.HttpRequestException
                        | :? OperationCanceledException
                        | :? AggregateException -> callback None
                    }
                , ignore
            )

        let internal get_async<'T> (route: string, callback: 'T option -> unit) : Async<unit> =
            queue.RequestAsync(
                fun client ->
                    async {
                        try
                            let! response = client.GetAsync(route) |> Async.AwaitTask

                            if response.IsSuccessStatusCode then
                                match response.Content.ReadAsStream() |> fun s -> JSON.FromStream(route, s) with
                                | Ok res -> callback (Some res)
                                | Error err ->
                                    Logging.Error(sprintf "Error getting %s: %s" route err.Message)
                                    callback None
                            else
                                callback None
                        with
                        | :? Http.HttpRequestException
                        | :? OperationCanceledException
                        | :? AggregateException -> callback None
                    }
            )

        let internal post_return<'T, 'U> (route: string, request: 'T, callback: 'U option -> unit) =
            queue.Request(
                fun client ->
                    async {
                        try
                            let! response =
                                client.PostAsync(
                                    route,
                                    new Http.StringContent(
                                        JSON.ToString request,
                                        Text.Encoding.UTF8,
                                        "application/json"
                                    )
                                )
                                |> Async.AwaitTask

                            if response.IsSuccessStatusCode then
                                match response.Content.ReadAsStream() |> fun s -> JSON.FromStream(route, s) with
                                | Ok res -> callback (Some res)
                                | Error err ->
                                    Logging.Error(sprintf "Error reading post %s: %s" route err.Message)
                                    callback None
                            else
                                callback None
                        with
                        | :? Http.HttpRequestException
                        | :? OperationCanceledException
                        | :? AggregateException -> callback None
                    }
                , ignore
            )

        let internal post<'T> (route: string, request: 'T, callback: bool option -> unit) =
            post_return<'T, bool> (route, request, callback)

        let internal post_async<'T, 'U> (route: string, request: 'T, callback: 'U option -> unit) =
            queue.RequestAsync(
                fun client ->
                    async {
                        try
                            let! response =
                                client.PostAsync(
                                    route,
                                    new Http.StringContent(
                                        JSON.ToString request,
                                        Text.Encoding.UTF8,
                                        "application/json"
                                    )
                                )
                                |> Async.AwaitTask

                            if response.IsSuccessStatusCode then
                                match response.Content.ReadAsStream() |> fun s -> JSON.FromStream(route, s) with
                                | Ok res -> callback (Some res)
                                | Error err ->
                                    Logging.Error(sprintf "Error reading post %s: %s" route err.Message)
                                    callback None
                            else
                                callback None
                        with
                        | :? Http.HttpRequestException
                        | :? OperationCanceledException
                        | :? AggregateException -> callback None
                    }
            )

        let internal delete (route: string, callback: bool option -> unit) =
            queue.Request(
                fun client ->
                    async {
                        try
                            let! response = client.DeleteAsync(route) |> Async.AwaitTask

                            if response.IsSuccessStatusCode then
                                match response.Content.ReadAsStream() |> fun s -> JSON.FromStream(route, s) with
                                | Ok res -> callback (Some res)
                                | Error err ->
                                    Logging.Error(sprintf "Error reading delete %s: %s" route err.Message)
                                    callback None
                            else
                                callback None
                        with
                        | :? Http.HttpRequestException
                        | :? OperationCanceledException
                        | :? AggregateException -> callback None
                    }
                , ignore
            )
