open System
open System.Net
open System.Net.Http
open System.Net.Security
open Percyqaz.Common
open Interlude.Web.Shared
open Interlude.Web.Tests.Client

Logging.Info "Make sure you have the server running"

let httpclient_handler = new HttpClientHandler()
httpclient_handler.ClientCertificateOptions <- ClientCertificateOption.Manual
httpclient_handler.ServerCertificateCustomValidationCallback <- fun _ cert _ sslPolicyErrors -> true
let httpclient = new HttpClient(httpclient_handler)

let get_debug_token_for_username(username: string) : string =
    httpclient.GetStringAsync(sprintf "https://127.0.0.1/auth/dummy?username=%s" (Uri.EscapeDataString username))
    |> Async.AwaitTask
    |> Async.RunSynchronously
    |> fun s -> s.Trim('"')

let clients : Client array = 
    [|
        EchoClient(get_debug_token_for_username "EchoClient")
        ExtraKeysClient(get_debug_token_for_username "ExtraKeysClient")
    |]

for c in clients do
    c.Connect()

Console.ReadLine() |> ignore
