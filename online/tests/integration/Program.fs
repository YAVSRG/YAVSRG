namespace Interlude.Web.Tests.Integration

open System.Net.Http
open Interlude.Web.Shared.API
open NUnit.Framework
open Prelude

[<SetUpFixture>]
type Setup() =

    [<OneTimeSetUp>]
    member _.InitAndAuth() =

        Client.http_client_handler.ServerCertificateCustomValidationCallback <- fun _ cert _ sslPolicyErrors -> true
        Client.init "https://localhost/"

        let http_client = new HttpClient(Client.http_client_handler)

        task {
            let! response = http_client.GetStringAsync("https://localhost/auth/dummy?username=Integration")

            let token =
                match JSON.FromString<string>(response) with
                | Ok t -> t
                | _ -> failwithf "Failed to get auth token from dummy endpoint, instead got %s" response

            Client.authenticate token
        }
        |> Async.AwaitTask
        |> Async.RunSynchronously

module Program =

    [<EntryPoint>]
    let main _ = 0